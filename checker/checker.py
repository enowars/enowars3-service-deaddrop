import json
import random
import string

import requests

from enochecker import BaseChecker, BrokenServiceException, run

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


def get_random_string(length: int = 10) -> str:
    return "".join(
        random.choice(string.ascii_uppercase + string.digits) for _ in range(length)
    )


class MessageQueueChecker(BaseChecker):
    port = 8080  # default port to send requests to.

    def putflag(self):
        tag = get_random_string()
        self.team_db[self.flag] = tag

        self.debug("Putting flag...")
        # XXX: Should we use tag or self.flag here?
        data = "- {}".format(tag)
        headers = {"Content-type": "application/x-www-form-urlencdoed"}
        response = self.http("PATCH", ADD_TOPIC_ENDPOINT, data=data, headers=headers)
        # XXX: Is checking for 200 enough?
        if response.status_code != 200:
            # TODO: Improve the error message.
            raise BrokenServiceException(
                "Broken service: could not put a flag ({})".format(self.flag)
            )
        self.debug("Flag {} up with tag: {}.".format(self.flag, tag))

    def getflag(self):
        self.http_get("/")

        try:
            tag = self.team_db[self.flag]
        except KeyError as ex:
            raise BrokenServiceException(
                "Inconsistent Database: Couldn't get tag for team/flag ({})".format(
                    self.flag
                )
            )

        r = self.http_get("/api/SearchAttacks", params={"needle": tag})
        self.info("Parsing search result")
        try:
            search_results = json.loads(r.text)
            id = search_results["matches"][0]["id"]
        except Exception as ex:
            raise BrokenServiceException(
                "Invalid JSON Response: {} ({})".format(r.text, ex)
            )

        self.info("Found attack (id={})".format(id))
        self.info("Fetching attack: {}".format({"id": id, "password": self.flag}))

        r = self.http_get(
            "/api/GetAttack",
            params={"id": id, "password": self.flag},
            timeout=5,
            verify=False,
        )
        self.info("Parsing GetAttack result")
        try:
            attack_results = json.loads(r.text)
        except Exception:
            raise BrokenServiceException("Invalid JSON: {}".format(r.text))

        try:
            flag_field = "attackDate" if self.flag_idx % 2 == 0 else "location"
            if attack_results["attack"][flag_field] != self.flag:
                raise BrokenServiceException(
                    "Incorrect flag in date field (searched for {} in {} - {})".format(
                        self.flag, attack_results, flag_field
                    )
                )
        except Exception as ex:
            if isinstance(ex, BrokenServiceException):
                raise
            raise BrokenServiceException(
                "Error parsing json: {}. {} (expected: {})".format(
                    attack_results, ex, self.flag
                )
            )

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
