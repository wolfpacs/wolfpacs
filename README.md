# WolfPACS

![Build status](https://github.com/wolfpacs/wolfpacs/actions/workflows/main.yml/badge.svg)
[![codecov.io](https://codecov.io/gh/wolfpacs/wolfpacs/coverage.svg?branch=master)](https://codecov.io/gh/wolfpacs/wolfpacs?branch=master)
[![Docker build](https://img.shields.io/docker/cloud/build/wolfpacs/wolfpacs.svg?color=green)](https://hub.docker.com/r/wolfpacs/wolfpacs)
[![License GPLv3](https://img.shields.io/badge/License-GPLv3-blue)](https://www.gnu.org/licenses)
[![Documentation](https://img.shields.io/badge/documentation-documentation-yellowgreen)](https://wolfpacs.github.io/wolfpacs/)
[![Version Observance](https://img.shields.io/badge/semver-0.4.0-blue)](https://semver.org/)

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

**WolfPACS** needs more black-box testing. If you have a use case please write raphexion+wolfpacs@gmail.com.

### Next milestone

- [ ] Support a secondary node which can take over or run in parallel to the main node.

## Bird's-eye view

Imagine a Medical AI company called __Stroke Insight__.
They have developed a cutting-edge algorithm to analyze MRIs; to detect strokes.
To analyze the images, they need computers with a lot of GPU power.
We call these machines **workers**.

These expensive computers are critical resources, that need serve many clients concurrently.
Moreover, once in a while, they need to take one or more workers offline in order to upgrade the software and/or the hardware.
To avoid announcing a service window, they want to be able to move load between workers in a safe way.

For this flexibility, they need and use **WolfPACS**.
Which can enable a pool of heterogeneous workers (hardware, software) to serve multiple clients in a flexible way.

### Image flow (primary and derived images)

Two clients, a hospital in Stockholm and one in Berlin are using __Stroke Insight's__ software.
Naturally, both the hospital in Stockholm and Berlin have their own central PACS systems on premises.
Whereas, __Stroke Insight__ has their computers in a data center.

### Incoming

```mermaid
flowchart LR

subgraph Stockholm
S_CLIENT[Radiologist] -->|Trigger| S_PACS[Local PACS]
end

S_PACS -->|Primary series| WP

subgraph Berlin
B_CLIENT[Radiologist] -->|Trigger| B_PACS[Local PACS]
end

B_PACS -->WP

subgraph WolfPACS
WP[Primary node]
SD[Secondary node]
end

subgraph Worker pool
WP-->|Series with the same StudyUID\nwill always end up on the same worker|WA[Worker C]
WB[Worker D]
end

subgraph Worker pool
WP-->WC[Worker A]
WD[Worker B]
end
```

1. A Radiologist sends the primary series to __Stroke Insight__ (which are running WolfPACS as a load balancer.)
2. WolfPACS receives the series and routes the images to an appropriate worker with the right software.

### Returning

``` mermaid
flowchart LR

subgraph Worker pool
WA[Worker A]
WB[Worker B]
end

subgraph Worker pool
WC[Worker C]
WD[Worker D]
end

WA-->|Derived series|WP
WC-->WP

subgraph WolfPACS
WP[Primary node]
SD[Secondary node]
end

WP-->|Route the derived series\nto the original sender|S_PACS
WP-->B_PACS

subgraph Berlin
B_PACS[Local PACS]-->B_CLIENT[Radiologist]
end

subgraph Stockholm
S_PACS[Local PACS]-->S_CLIENT[Radiologist]
end
```

3. The worker sends the new derived series back to WolfPACS.
4. Finally, WolfPACS sends the new series back to the correct destination.

## Mental model for WolfPACS administration

Any router / load balancer has two sides.
One side facing the outside world.
And the other side facing the inside world (workers).

We expose port 11112 for outside clients of WolfPACS.
Workers on the other side, should contact WolfPACS on port 11113.

Therefore, if you deploy WolfPACS, you need to expose 11112 to the outside world.
Whereas you want to keep 11113 open inside the firewall (trusted side).

![Logo](priv/mental-model.png)

## Client vs Destination

A client is anyone with the correct Application Entity (AE). This acts as a shared secret / password.

A destination is a server that can receive DICOM data.
WolfPACS needs a hostname, an IP-address and a called AE.

So the client will send data to WolfPACS and the destination will receive data from WolfPACS.

## Quick Start

Start WolfPACS in background.

```sh
docker run -d -p 11112:11112 -p 11113:11113 wolfpacs/wolfpacs
```

Debug WolfPACS instance

```sh
docker run -it -p 11112:11112 -p 11113:11113 wolfpacs/wolfpacs console
```

## Environment variables

It is possible to configure some parts of WolfPACS using environmental variables.

| Variable              | Description                           | Default |
|-----------------------|---------------------------------------|---------|
| WOLFPACS_INSIDE_PORT  | The port towards the workers          | 11112   |
| WOLFPACS_OUTSIDE_PORT | The port facing the outside world     | 11113   |

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
