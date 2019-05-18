import asyncio
import json
import random
import string

import requests

import websockets
from enochecker import BaseChecker, BrokenServiceException, run, sha256ify

session = requests.Session()


class MessageQueueChecker(BaseChecker):
    port = 9090  # default port to send requests to.

    @property
    def greeting(self):
        return "Heyhey from WS Handler.."

    # /publish
    # * POST
    # * Payload: `Topic 1:message_string`
    # * Sends call to event handler `file_handler` to save received msg to file
    #   `Topic 1_msg_save.txt`
    @property
    def publish_endpoint(self):
        return "/publish"

    # /subscribe
    # * Upgrades to Websocket automatically
    # * When connected use `SUBSCRIBE: topicname` to subscribe to topics
    # * Use `REPLAY: topicname` to receive all messages sent to Topic `topicname`
    @property
    def subscribe_endpoint(self):
        return "/subscribe"

    # /topics
    # * GET
    @property
    def topics_endpoint(self):
        return "/topics"

    # `/add_topic`
    # * PATCH
    # * Payload:
    #     * `- topicname` for private topics
    #     * `+ topicname` or `topicname` for public topics
    # * Creates file `topicname_msg_save.txt`. The file is used to store
    #   messages sent to this topic
    @property
    def add_topic_endpoint(self):
        return "/add_topic"

    # Send a replay request for the given topic to the subscribe endpoint.
    def replay(self, topic: str) -> str:
        async def request(topic: str):
            async with websockets.connect(
                f"ws://{self.address}:{self.port}{self.subscribe_endpoint}"
            ) as websocket:
                # Ignore the greeting message.
                greeting = await websocket.recv()

                if greeting != self.greeting:
                    raise BrokenServiceException(
                        f"Broken service: /subscribe endpoint greeted us with: {greeting}"
                    )

                # Request to replay the topic with the flag.
                await websocket.send(f"REPLAY: {topic}")

                # Receive all the messages relateed to the requested topic.
                return await websocket.recv()

        return asyncio.get_event_loop().run_until_complete(request(topic))

    def add_private_topic(self, topic):
        return self.http("PATCH", self.add_topic_endpoint, data="- {topic}")

    def putflag(self):
        topic = sha256ify(self.flag)
        self.debug(f'Putting flag "{self.flag}" to topic "{topic}"...')
        response = self.add_private_topic(topic)
        if response.status_code != 200:
            raise BrokenServiceException(
                f'Broken service: could not put flag "{self.flag}" to topic "{topic}"'
            )
        self.debug(f'Flag "{self.flag}" put')

    def getflag(self):
        topic = sha256ify(self.flag)
        self.debug(f'Getting flag "{self.flag}" from topic "{topic}"...')
        response = self.replay(topic)
        if response == "Unknown Topic.":
            raise BrokenServiceException(
                f'Broken service: topic "{topic}" with flag "{self.flag}" is unknown to the service'
            )
        self.debug(f'Flag "{self.flag}" got')

    def putnoise(self):
        pass

    def getnoise(self):
        pass

    def havoc(self):
        response = self.http_get("/topics")
        if response.text != "Bad Request.":
            raise BrokenServiceException(
                f"Unexpected reponse from the /topics endpoint: {response.text}"
            )


app = MessageQueueChecker.service
if __name__ == "__main__":
    run(MessageQueueChecker)
