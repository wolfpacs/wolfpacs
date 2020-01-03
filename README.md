WolfPACS
========

[![Build Status](https://travis-ci.org/wolfpacs/wolfpacs.svg?branch=master)](https://travis-ci.org/wolfpacs/wolfpacs)
[![codecov.io](https://codecov.io/gh/wolfpacs/wolfpacs/coverage.svg?branch=master)](https://codecov.io/gh/wolfpacs/wolfpacs?branch=master)
[![Docker build](https://img.shields.io/docker/cloud/build/wolfpacs/wolfpacs.svg?color=green)](https://hub.docker.com/r/wolfpacs/wolfpacs)
[![License Apache 2](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

WolfPACS is an open-source Picture Archiving and Communication System (PACS) solution written in Erlang.

DICOM Conformance Statement
---------------------------

The following transfer syntax are are supported:

| Transfer Syntax           | UID                 | Supported |
|---------------------------|---------------------|-----------|
| Implicit VR Little Endian | 1.2.840.10008.1.2   | Yes       |
| Explicit VR Little Endian | 1.2.840.10008.1.2.1 | No        |
| Explicit VR Big Endian    | 1.2.840.10008.1.2.2 | No        |

The following services are supported:

| Name        | UID               | SCP | SCU |
|-------------|-------------------|-----|-----|
|Verification | 1.2.840.10008.1.1 | Yes | NO  |

Links and references
--------------------

[How to Write Erlang Documentation](https://docs.2600hz.com/dev/doc/engineering/erlang-documentation/)
