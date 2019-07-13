Deaddrop service
================

General description
-------------------

This service is publish/subscriber message queue written in [Erlang](https://www.erlang.org/). It was created for the [ENOWARS 3](https://enowars.com/) A/D-CTF.

Vulnerabiliy
---------
The vulnerability included in the service is connected with the topic replay function. Actual exploit code can be found in [checker/checker.py](checker/checker.py).  
Internally all messages that are sent through the service are saved to a simple text file. There is one file per topic. There is also one file called `topics.txt` that holds all topics that are currently available. The `topics.txt` file is used with the `/topics` endpoint to serve the currently available topics. The topics files are used to enable the replay feature. For this the content of the files is read and printed to the websocket connection requesting the replay. If one requests a replay of the private topic `- topics`, one receives all topics, not only the public one published through the `/topics` endpoint. By knowing all private topics one is able to replay those and retrieve flags that were published to those topics.

Releasing
---------

The release branch has become obsolete by now. Releases are now handled by tagging a commit with a version number following the [semantic versioning scheme](https://semver.org/).  
The included release makefile can still be used for inspiration.

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

-	Persistance files will regularly be compressed to save space.
-	Partitions of topics allow to parallelize message handling over different clients.
-	Private topics require authentication. 

### Bug Ideas

-	Some kind of path traversal when re-sending messages?
-	Overflowing the log files?

References
----------

-	https://github.com/erlang/docker-erlang-example
