from threading import Thread, Lock
from pydicom import dcmread
from pynetdicom import AE
from pynetdicom.sop_class import CTImageStorage
from pydicom.uid import (ImplicitVRLittleEndian,
                         ExplicitVRLittleEndian,
                         ExplicitVRBigEndian)


def _send_ctimage(hostname, port, transfersyntax):
    ae = AE()
    ae.add_requested_context(CTImageStorage, transfersyntax)
    assoc = ae.associate(hostname, int(port))
    return assoc


def send_ctimage_implicit_little(hostname, port):
    return _send_ctimage(hostname, port, ImplicitVRLittleEndian)


def send_ctimage_explicit_little(hostname, port):
    return _send_ctimage(hostname, port, ExplicitVRLittleEndian)


def send_ctimage_explicit_big(hostname, port):
    return _send_ctimage(hostname, port, ExplicitVRBigEndian)


def is_established(assoc):
    return assoc.is_established


def send_image(assoc, filename):
    ds = dcmread(filename)
    status = assoc.send_c_store(ds)
    return status.Status == 0x0000


def release(assoc):
    return assoc.release()
