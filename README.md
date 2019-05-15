# Message queue service

## General description

This service will be a message queue much like [Apache Kafka](https://kafka.apache.org/). The core functionality will be written in [Erlang](https://www.erlang.org/).

Run `make docker-clean` to stop and destroy all Docker containers running on the system. Run `make docker-build` to build a new container called `msq`. Run `make docker-run` to start a Docker running the service. 

## Service

### Dependencies

- Erlang 21

### Testing with shell scripts

Dependencies:

- cURL
- [websocat](https://github.com/vi/websocat)

Running tests:

```sh
make rel
make test
```

### Testing with Docker and checker

```sh
# Start the service listening at localhost:8080
make docker-clean docker-build docker-run

# Install checker dependencies
pip install git+https://github.com/domenukk/enochecker
python3 checker/checker.py run havoc
```

Alternatively:

Run `docker-compose up -d` to start the service and `docker-compose down` to stop it.

### Notes

- Listening port: 8080

#### Erlang Relevant Folders in /service

These are needed for the `make run` to succeed:

  * `_rel`: Binaries created by erlang.mk
  * `.erlang.mk`: Created by erlang.mk
  * `config`: Created when initializing project with cowboy
  * `deps`: Dependencies created by erlang.mk
  * `ebin`: Erlang bytecode
  * `src`: Sourcecode
  * `test`: Tests

#### Endpoints

  - `/publish` 
    * POST 
    * Headers -- Content-type: application/x-www-form-urlencoded
    * Payload: `Topic 1:message_string`
    * Sends call to event handler `file_handler` to save received msg to file `Topic 1_msg_save.txt`
  - `/subscribe`
    * Upgrades to Websocket automatically
    * When connected use `SUBSCRIBE: topicname` to subscribe to topics
    * Use `REPLAY: topicname` to receive all messages sent to Topic `topicname`
  - `/topics` 
    * GET 
  - `/add_topic` 
    * PATCH 
    * Headers -- Content-type: application/x-www-form-urlencdoed
    * Payload: 
        * `- topicname` for private topics
        * `+ topicname` or `topicname` for public topics
    * Creates file `topicname_msg_save.txt`. The file is used to store messages sent to this topic

## Other processes

- `subscriber_pool`: A server process that saves all subscribers within its state. The state contains a single dict that has topics as keys and lists of PIDs as values. Each PID represents the WS connection to the subscriber of a topic. Casts (async) are used by the `/publish` endpoint to notify the `subscriber_pool` of incoming messages.

- `file_handler`: Event handler used to access message save files. As there is no concept of locks within Erlang a single process is used to access the message files. Notifys (async) are used to read from the files. Calls (sync) are used to write to the files.


#### Feature Ideas

* Clients will be able to subscribe and publish messages to topics. Every subscriber of a topic will receive messages published to said topic. 
* Messages will be persisted into files. By saving an offset a subscriber will be able to re-read/re-send messages. 
* Persistance files will regularly be compressed to save space.
* Partitions of topics allow to parallelize message handling over different clients.
* A REST-API to interact with the queue.
* Secret topics that require authentication. This has to be included at least once w/o bugs to enable the checker to work.

#### Bug Ideas

* A message triggers a flag to be published to a "flag" topic. Fixable by removing the flag from the triggered message. The trigger-message can be crafted by understanding some random algorithm within the source.
* Some kind of path traversal when re-sending messages?
* Overflowing the log files?
* ___________________

## Other notes

* socket_server.erl is based on code provided by 'Jesse E.I. Farmer <jesse@20bits.com>'

### References

- https://github.com/erlang/docker-erlang-example
