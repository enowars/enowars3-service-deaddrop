import asyncio
import json
import random
import string
import sys

import requests

import websocket
from enochecker import BaseChecker, BrokenServiceException, run, sha256ify

session = requests.Session()


def generate_topic(s):
    return sha256ify(s)


class MessageQueueChecker(BaseChecker):
    port = 8080  # default port to send requests to.

    flag_count = 1
    noise_count = 1
    havoc_count = 1

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
    def replay(self, topic: str, private=True) -> str:
        self.debug(f'Replaying topic "{topic}"...')

        if private:
            topic_prefix = "- "
        else:
            topic_prefix = "+ "
        self.debug(f"Connecting to the websocket...")
        socket = websocket.create_connection(
            f"ws://{self.address}:{self.port}{self.subscribe_endpoint}"
        )
        self.debug(f"Receiving greeting...")
        greeting = socket.recv()
        if self.greeting != greeting:
            raise BrokenServiceException(
                f'Endpoint "/subscribe" greeted us with: {greeting}'
            )
        self.debug(f'Requesting topic "{topic}" to be replayed...')
        # Request to replay the topic with the flag.
        socket.send(f"REPLAY:{topic_prefix}{topic}")

        # Receive all the messages related to the requested topic.
        messages = socket.recv()
        self.debug(f'Replayed messages are: "{messages}"')

        # XXX: Is it alright to just close the socket here? Could it be
        # that the socket is not closed if we abort earlier?
        socket.close()

        return messages

    def add_private_topic(self, topic):
        return self.http("PATCH", self.add_topic_endpoint, data=f"- {topic}")

    def add_public_topic(self, topic):
        return self.http("PATCH", self.add_topic_endpoint, data=f"+ {topic}")

    def publish(self, topic, message):
        return self.http_post(self.publish_endpoint, data=f"{topic}:{message}")

    def list_topics(self):
        return self.http_get(self.topics_endpoint)

    def must_publish_to_new_topic(self, topic, message, private=True):
        if private:
            self.debug(f'Publishing message "{message}" to private topic "{topic}"')
            response = self.add_private_topic(topic)
        else:
            self.debug(f'Publishing message "{message}" to public topic "{topic}"')
            response = self.add_public_topic(topic)
        if response.status_code != 200:
            raise BrokenServiceException(f'Could not add topic "{topic}"')
        publish_response = self.publish(topic, message)
        if (
            publish_response.status_code != 200
            or publish_response.text is "Error returned by notify_subscribers."
        ):
            raise BrokenServiceException(
                f'Could not publish message "{message}" to topic "{topic}"'
            )

        self.debug(f'Message "{message}" published')

    def must_replay(self, topic):
        response = self.replay(topic)
        if response == "Unknown Topic.":
            raise BrokenServiceException(
                f'Unable to replay topic "{topic}": topic unknown'
            )
        return response

    def must_get_message(self, topic, message):
        response = self.must_replay(topic)
        if response.find(message) == -1:
            raise BrokenServiceException(
                f'Message "{message}" missing from replay of topic "{topic}"'
            )

        self.debug(f'Message "{message}" got')

    def must_list_topics(self):
        self.debug(f"Listing topics...")
        response = self.list_topics()
        if response.status_code != 200:
            raise BrokenServiceException(
                f"Unexpected status code from the /topics endpoint: {response.status_code}"
            )
        self.debug(f"Topics listed")
        return response.text

    def putflag(self):
        topic = generate_topic(self.flag)
        self.debug(f'Putting flag "{self.flag}" to topic "{topic}"...')
        self.must_publish_to_new_topic(topic, self.flag)
        self.debug(f'Flag "{self.flag}" put')

    def getflag(self):
        topic = generate_topic(self.flag)
        self.debug(f'Getting flag "{self.flag}" from topic "{topic}"...')
        self.must_get_message(topic, self.flag)
        self.debug(f'Flag "{self.flag}" got')

    def putnoise(self):
        topic = generate_topic(self.noise)
        self.debug(f'Putting noise "{self.noise}" to topic "{topic}"...')
        self.must_publish_to_new_topic(topic, self.noise)
        self.debug(f'Noise "{self.noise}" put')

    def getnoise(self):
        topic = generate_topic(self.noise)
        self.debug(f'Getting noise "{self.noise}" from topic "{topic}"...')
        self.must_get_message(topic, self.noise)
        self.debug(f'Noise "{self.noise}" got')

    def havoc(self):
        topics = self.must_list_topics()

    def exploit(self):
        desired_topic = generate_topic(self.flag)
        exploitable_topic = "topics"
        all_topics = self.must_replay(exploitable_topic)
        self.debug(f'Received topics are: "{all_topics}"')
        if all_topics.find(desired_topic) == -1:
            raise BrokenServiceException(
                f'Topic "{desired_topic}" missing from replay "{all_topics}" of topic "{exploitable_topic}"'
            )
        self.must_get_message(desired_topic, self.flag)
        self.debug("Service exploited")


app = MessageQueueChecker.service
if __name__ == "__main__":
    sys.exit(run(MessageQueueChecker))
