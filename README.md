# WolfPACS

[![Build Status](https://travis-ci.org/wolfpacs/wolfpacs.svg?branch=master)](https://travis-ci.org/wolfpacs/wolfpacs)
[![codecov.io](https://codecov.io/gh/wolfpacs/wolfpacs/coverage.svg?branch=master)](https://codecov.io/gh/wolfpacs/wolfpacs?branch=master)
[![Docker build](https://img.shields.io/docker/cloud/build/wolfpacs/wolfpacs.svg?color=green)](https://hub.docker.com/r/wolfpacs/wolfpacs)
[![License Apache 2](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

![Logo](priv/logo.png)

## Raison d'être

With the advent of powerful AI solutions in Radiology,
there is growing need to split the workload across multiple workers.
**WolfPACS** acts as a load balancer, sending DICOM series to the correct worker.

## Mission statement

> Enable a pool of heterogeneous workers (hardware, software) to serve multiple clients in a flexible way.

## Status

**WolfPACS** is currently in the **Alpha** phase of development.
Some critical bugs may still remain in the software.

**WolfPACS** is close to feature freeze but is open for smaller adjustments upon feedback.

**WolfPACS** needs more black-box testing. If you have a use case please write raphexion+wolfpacs@gmail.com.

## Bird's-eye view

Imagine two hospitals that need help with processing data.
Let's call them Stockholm Hospital (S) and Berlin Hospital (B).
Both Stockholm and Berlin have their own central PACS systems.
Let's call them S-PACS and B-PACS.

A medical company provides a software solution, that provides extra information
(derived series) to a medical study. Their software is running on external
computers (called workers).

![Logo](priv/dream1.png)

Steps in figure above.

1. A Radiologist sends the primary series to WolfPACS.
2. WolfPACS receives the series, routes the data to the correct worker.
3. The worker sends the new derived series back to WolfPACS.
4. Finally, WolfPACS sends then new series to the correct destination.

## Mental model

Any router / load balancer has two sides.
On side facing the outside world.
And the other side facing the inside world (workers).

We expose port 11112 for outside clients of WolfPACS.
Workers on the other side, should contact WolfPACS on port 11113.

Therefore, if you deploy WolfPACS, you need to expose 11112 to the outside world.
Whereas you want to keep 11113 open inside the firewall (trusted side).

In addtion, WolfPACS is best configured using http.
WolfPACS listens on port 8080.

Please see ![mini_admin.py](priv/mini_admin.py) for an example python script.

## Quick Start

Start WolfPACS in background.

```sh
docker run -d -p 11112:11112 -p 11113:11113 -p 8080:8080 wolfpacs/wolfpacs
```

Debug WolfPACS instance

```sh
docker run -it -p 11112:11112 -p 11113:11113 -p 8080:8080 wolfpacs/wolfpacs console
```

## DICOM Conformance Statement

The following transfer syntax are are supported:

| Transfer Syntax           | UID                 | Supported |
| ------------------------- | ------------------- | --------- |
| Implicit VR Little Endian | 1.2.840.10008.1.2   | Yes       |
| Explicit VR Little Endian | 1.2.840.10008.1.2.1 | Yes       |
| Explicit VR Big Endian    | 1.2.840.10008.1.2.2 | Yes       |

## Test plan

A PACS is classified as a medical device and needs to be painstakingly tested.

We use four different test in WolfPACS and we aim to test the software thoroughly.

| Test                   | Target                | Method                                                                           |
| ---------------------- | --------------------- | -------------------------------------------------------------------------------- |
| Unit tests             | One Module            | [Erlang Eunit](http://erlang.org/doc/apps/eunit/chapter.html)                    |
| Integration tests      | Many Modules          | [Erlang Common Tests](https://erlang.org/doc/apps/common_test/introduction.html) |
| Validation testing     | User requirements     | [Python Robot Framework](https://robotframework.org/)                            |
| Property based testing | Hidden bugs / Fussing | [Erlang proper](https://propertesting.com/)                                      |


