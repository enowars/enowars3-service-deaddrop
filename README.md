Deaddrop service
================

General description
-------------------

This service will be a message queue much like [Apache Kafka](https://kafka.apache.org/). The core functionality will be written in [Erlang](https://www.erlang.org/).

CI: https://ci.eno.host/browse/SER-MESS

Releasing
---------

### Updating the release branch with the latest service code from master

```sh
# Optionally, clean up any remainings after previous releases.
make -f Makefile.release release-clean

make -f Makefile.release release-clone
make -f Makefile.release release-update

# Optionally, try to build and run the container.
make -f Makefile.release release-qa

make -f Makefile.release release-push
env VERSION=0.1.0 make -f Makefile.release release-tag

make -f Makefile.release release-clean
```

If you are brave enough you may also decide to use `env VERSION=0.1.0 make -f Makefile.release release`, which does all the necessary steps mentioned above. Just make sure that the version is unique and increasing.

### Integrating release branch hotfixes back to master

```sh
make -f Makefile.release sync-release-to-master
```

Service
-------

Start the service with:

```sh
cd service
docker-compose up -d
```

Stop the service with:

```sh
docker-compose down
```

Development
-----------

See [service/DEVELOPMENT.md](service/DEVELOPMENT.md) for service devlopment notes.

See [Vagrantfile](Vagrantfile) to figure out what dependencies and provisioning are needed.

### Testing

#### Dependencies

Required tools:

-	bmake or GNU make
-	pip3

Installing dependencies:

```sh
pip3 install -r checker/requirements.txt
```

#### Running Tests

```sh
make test
```

### Debugging

-	Run `docker attach service_queue_1` in a second terminal to gather crash reports from the Erlang app (for example due to an invalid HTTP request being received).

Ideas
-----

### Feature Ideas

-	Clients will be able to subscribe and publish messages to topics. Every subscriber of a topic will receive messages published to said topic.
-	Messages will be persisted into files. By saving an offset a subscriber will be able to re-read/re-send messages.
-	Persistance files will regularly be compressed to save space.
-	Partitions of topics allow to parallelize message handling over different clients.
-	A REST-API to interact with the queue.
-	Secret topics that require authentication. This has to be included at least once w/o bugs to enable the checker to work.

### Bug Ideas

-	A message triggers a flag to be published to a "flag" topic. Fixable by removing the flag from the triggered message. The trigger-message can be crafted by understanding some random algorithm within the source.
-	Some kind of path traversal when re-sending messages?
-	Overflowing the log files?

Credits
-------

-	socket_server.erl is based on code provided by 'Jesse E.I. Farmer jesse@20bits.com'

References
----------

-	https://github.com/erlang/docker-erlang-example
