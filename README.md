Deaddrop service
================

General description
-------------------

This service is a publish/subscriber message queue written in [Erlang](https://www.erlang.org/). It was created for the [ENOWARS 3](https://enowars.com/) attack/defense CTF.

Vulnerability
-------------

The vulnerability included in the service is connected with the topic replay function. Actual exploit code can be found in [checker/checker.py](checker/checker.py).

Internally all messages that are sent through the service are saved to a simple text file. There is one file per topic. There is also one file called `topics.txt` that holds all topics that are currently available. The `topics.txt` file is used with the `/topics` endpoint to serve the currently available topics. The topic files are used to enable the replay feature. For this the content of the files is read and printed to a WebSocket connection requesting the replay. It is possible to retrieve all topics (not only the public ones available via the `/topics` endpoint) if a replay of the private topic `- topics` is requested. As a result, one is able to replay private topics to get the flags.

### Write-ups

-	https://saarsec.rocks/2019/07/12/deaddrop.html

Releasing
---------

Releases are now handled by tagging a commit with a version number following the [semantic versioning scheme](https://semver.org/).

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

-	Persistence files will regularly be compressed to save space.
-	Partitions of topics allow to parallelize message handling over different clients.
-	Private topics require authentication.

### Bug Ideas

-	Some kind of path traversal when re-sending messages?
-	Overflowing the log files?
