# Description

This service will be a message queue much like [Apache Kafka](https://kafka.apache.org/). The core functionality will be written in [Erlang](https://www.erlang.org/).

## Dependencies

- Erlang 21

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
