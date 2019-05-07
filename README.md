# Description

This service will be a message queue much like [Apache Kafka](https://kafka.apache.org/). The core functionality will be written in [Erlang](https://www.erlang.org/).

Run `make docker-clean` to stop and destroy all Docker containers running on the system. Run `make docker-build` to build a new container called `msq`. Run `make docker-run` to start a Docker running the service. 

## Dependencies

- Erlang 21

# Notes about the service

- Listening port: 8080

## Endpoints
  - `/publish` 
    * POST 
    * Headers -- Content-type: application/x-www-form-urlencoded
    * Payload: `Topic 1:message_string`
  - `/subscribe`
    * Upgrades to Websocket automatically
    * When connected use `SUBSCRIBE: topicname` to subscribe to topics.
  - `/topics` 
    * GET 
  - `/add_topic` 
    * PATCH 
    * Headers -- Content-type: application/x-www-form-urlencdoed
    * Payload: 
        * `- topicname` for private topics
        * `+ topicname` or `topicname` for public topics

## Testing

```sh
make rel
make test
```

### Dependencies

- cURL
- [websocat](https://github.com/vi/websocat)

# Feature Ideas

* Clients will be able to subscribe and publish messages to topics. Every subscriber of a topic will receive messages published to said topic. 
* Messages will be persisted into files. By saving an offset a subscriber will be able to re-read/re-send messages. 
* Persistance files will regularly be compressed to save space.
* Partitions of topics allow to parallelize message handling over different clients.
* A REST-API to interact with the queue.
* Secret topics that require authentication. This has to be included at least once w/o bugs to enable the checker to work.

# Bug Ideas

* A message triggers a flag to be published to a "flag" topic. Fixable by removing the flag from the triggered message. The trigger-message can be crafted by understanding some random algorithm within the source.
* Some kind of path traversal when re-sending messages?
* Overflowing the log files?
* ___________________

# Notes

* socket_server.erl is based on code provided by 'Jesse E.I. Farmer <jesse@20bits.com>'

## References

- https://github.com/erlang/docker-erlang-example
