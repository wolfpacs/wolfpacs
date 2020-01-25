from threading import Thread, Lock
from pynetdicom import AE
from pydicom.uid import (ImplicitVRLittleEndian,
                         ExplicitVRLittleEndian,
                         ExplicitVRBigEndian)


def get_implicit_assoc(host, port):
    ae = AE(ae_title=b'robot')
    ae.acse_timeout = 2
    ae.dimse_timeout = 2
    ae.network_timeout = 2
    ae.add_requested_context('1.2.840.10008.1.1', ImplicitVRLittleEndian)
    assoc = ae.associate(host, int(port))
    return assoc


def get_explicit_assoc(host, port):
    ae = AE(ae_title=b'robot')
    ae.acse_timeout = 2
    ae.dimse_timeout = 2
    ae.network_timeout = 2
    ae.add_requested_context('1.2.840.10008.1.1', ExplicitVRLittleEndian)
    assoc = ae.associate(host, int(port))
    return assoc


def get_explicit_big_assoc(host, port):
    ae = AE(ae_title=b'robot')
    ae.acse_timeout = 2
    ae.dimse_timeout = 2
    ae.network_timeout = 2
    ae.add_requested_context('1.2.840.10008.1.1', ExplicitVRBigEndian)
    assoc = ae.associate(host, int(port))
    return assoc


def is_established(assoc):
    return assoc.is_established


def send_c_echo(assoc):
    return assoc.send_c_echo()


def release(assoc):
    return assoc.release()
