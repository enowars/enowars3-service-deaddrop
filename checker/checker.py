import asyncio
import json
import random
import string

import requests

import websockets
from enochecker import BaseChecker, BrokenServiceException, run, sha256ify

session = requests.Session()


# /publish
# * POST
# * Headers -- Content-type: application/x-www-form-urlencoded
# * Payload: `Topic 1:message_string`
# * Sends call to event handler `file_handler` to save received msg to file
#   `Topic 1_msg_save.txt`
PUBLISH_ENDPOINT = "/publish"
# /subscribe
# * Upgrades to Websocket automatically
# * When connected use `SUBSCRIBE: topicname` to subscribe to topics
# * Use `REPLAY: topicname` to receive all messages sent to Topic `topicname`
SUBSCRIBE_ENDPOINT = "/subscribe"
# /topics
# * GET
TOPICS_ENDPOINT = "/topics"
# `/add_topic`
# * PATCH
# * Headers -- Content-type: application/x-www-form-urlencdoed
# * Payload:
#     * `- topicname` for private topics
#     * `+ topicname` or `topicname` for public topics
# * Creates file `topicname_msg_save.txt`. The file is used to store
#   messages sent to this topic
ADD_TOPIC_ENDPOINT = "/add_topic"


class MessageQueueChecker(BaseChecker):
    port = 8080  # default port to send requests to.

    def putflag(self):
        self.debug("Putting flag...")
        data = "+ {}".format(self.flag)
        response = self.http("PATCH", ADD_TOPIC_ENDPOINT, data=data)
        # XXX: Is checking for 200 enough?
        if response.status_code != 200:
            # TODO: Improve the error message.
            raise BrokenServiceException(
                "Broken service: could not put a flag ({})".format(self.flag)
            )
        self.debug("Flag put ({})".format(self.flag))

    def getflag(self):
        async def hello():
            async with websockets.connect(
                f"ws://{self.address}:{self.port}{SUBSCRIBE_ENDPOINT}"
            ) as websocket:
                # Ignore the greeting message.
                await websocket.recv()

                # Request to replay the topic with the flag.
                await websocket.send(f"REPLAY: {self.flag}")

                # Receive all the messages relateed to the requested topic.
                response = await websocket.recv()
                if response == "Unknown Topic.":
                    raise BrokenServiceException(
                        "Broken service: the topic with the flag ({}) is unknown to the service".format(
                            self.flag
                        )
                    )

        asyncio.get_event_loop().run_until_complete(hello())

    def putnoise(self):
        pass

    def getnoise(self):
        pass

    def havoc(self):
        response = self.http_get("/topics")
        if response.text != "No topics created yet.":
            raise BrokenServiceException(
                'Unexpected reponse from the /topics endpoint: "{}"'.format(
                    response.text
                )
            )


app = MessageQueueChecker.service
if __name__ == "__main__":
    run(MessageQueueChecker)
