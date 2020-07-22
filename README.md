# WolfPACS

[![Build Status](https://travis-ci.org/wolfpacs/wolfpacs.svg?branch=master)](https://travis-ci.org/wolfpacs/wolfpacs)
[![codecov.io](https://codecov.io/gh/wolfpacs/wolfpacs/coverage.svg?branch=master)](https://codecov.io/gh/wolfpacs/wolfpacs?branch=master)
[![Docker build](https://img.shields.io/docker/cloud/build/wolfpacs/wolfpacs.svg?color=green)](https://hub.docker.com/r/wolfpacs/wolfpacs)
[![License Apache 2](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

![Logo](priv/wolfpacs_small.png)

WolfPACS is an open-source Picture Archiving and Communication System (PACS) solution written in Erlang.

## Status

**WolfPACS** is under active development and not ready for production.

## Test plan

A PACS is classified as a medical device and needs to be painstakingly tested.

We use four different test in WolfPACS and we aim to test the software thoroughly.

| Test                   | Target                | Method                                                                           |
| ---------------------- | --------------------- | -------------------------------------------------------------------------------- |
| Unit tests             | One Module            | [Erlang Eunit](http://erlang.org/doc/apps/eunit/chapter.html)                    |
| Integration tests      | Many Modules          | [Erlang Common Tests](https://erlang.org/doc/apps/common_test/introduction.html) |
| Validation testing     | User requirements     | [Python Robot Framework](https://robotframework.org/)                            |
| Property based testing | Hidden bugs / Fussing | [Erlang proper](https://propertesting.com/)                                      |

## Quick Start

Start WolfPACS in background.

```sh
docker run -d -p 11112:11112 wolfpacs/wolfpacs
```
Debug WolfPACS instance

```sh
docker run -it -p 11112:11112 wolfpacs/wolfpacs console
```

## Configuration

```
# wolfpacs.conf

# Add worker (s)
# A worker is an "internal" machine that may do some processing on DICOM files.
# The specification is: {worker, <host>, <port>, <ae>}
# The worker should reply to wolfpacs on port 11113 (default).

{worker, "localhost", 1234, "WORKER"}.

# Add destination(s)
# A destination is an "external" machine that will recieve the final
# series that where created by the worker.
# The specification is: {destination, {<called AE>, <calling AE>}, <destination-host>, <destination-port>}
# The destination should be prepared to answer and receive data on <host> and <port>.
# The <called AE> and <calling AE> acts as a "key"/"authorization"/"routing" pair.
# Especially, it is the original call to wolfpacs that uses this pair.
#
# dcmsend -aec WOLFPACS -aet DCMSEND <wolfpacs-host> <wolfpacs-port>

{destination, {"WOLFPACS", "DCMSEND"}, "pacs.example.com", 11112}.
```

```sh
docker run -d -v /a/path/config/folder:/app123 -e WOLFPACS_DIR=/app123 -p 11112:11112 wolfpacs/wolfpacs
```

## DICOM Conformance Statement

The following transfer syntax are are supported:

| Transfer Syntax           | UID                 | Supported |
| ------------------------- | ------------------- | --------- |
| Implicit VR Little Endian | 1.2.840.10008.1.2   | Yes       |
| Explicit VR Little Endian | 1.2.840.10008.1.2.1 | Yes       |
| Explicit VR Big Endian    | 1.2.840.10008.1.2.2 | Yes       |

The following services are supported:

| Name         | UID               | SCP       | SCU       |
| ------------ | ----------------- | --------- | --------- |
| Verification | 1.2.840.10008.1.1 | Yes (PoC) | Yes (PoC) |

## Links and references

[How to Write Erlang Documentation](https://docs.2600hz.com/dev/doc/engineering/erlang-documentation/)

[DICOM Validator](https://www.dclunie.com/dicom3tools/dciodvfy.html)
