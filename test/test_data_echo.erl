-module(test_data_echo).
-export([c_echo_scu/0]).

c_echo_scu() ->
    %% $ echoscu -aet bbbbbb -v -d localhost 11112
    %% D: $dcmtk: echoscu v3.6.4 2018-11-29 $
    %% D:
    %% D: DcmDataDictionary: Loading file: /usr/share/libdcmtk14/dicom.dic
    %% D: DcmDataDictionary: Loading file: /usr/share/libdcmtk14/private.dic
    %% D: Request Parameters:
    %% D: ====================== BEGIN A-ASSOCIATE-RQ =====================
    %% D: Our Implementation Class UID:      1.2.276.0.7230010.3.0.3.6.4
    %% D: Our Implementation Version Name:   OFFIS_DCMTK_364
    %% D: Their Implementation Class UID:
    %% D: Their Implementation Version Name:
    %% D: Application Context Name:    1.2.840.10008.3.1.1.1
    %% D: Calling Application Name:    bbbbbb
    %% D: Called Application Name:     ANY-SCP
    %% D: Responding Application Name: ANY-SCP
    %% D: Our Max PDU Receive Size:    16384
    %% D: Their Max PDU Receive Size:  0
    %% D: Presentation Contexts:
    %% D:   Context ID:        1 (Proposed)
    %% D:     Abstract Syntax: =VerificationSOPClass
    %% D:     Proposed SCP/SCU Role: Default
    %% D:     Proposed Transfer Syntax(es):
    %% D:       =LittleEndianImplicit
    %% D: Requested Extended Negotiation: none
    %% D: Accepted Extended Negotiation:  none
    %% D: Requested User Identity Negotiation: none
    %% D: User Identity Negotiation Response:  none
    %% D: ======================= END A-ASSOCIATE-RQ ======================
    %% I: Requesting Association
    %% D: setting network send timeout to 60 seconds
    %% D: setting network receive timeout to 60 seconds
    %% D: Constructing Associate RQ PDU
    %% F: Association Request Failed: 0006:031a DUL network read timeout

    %%
    %% 1.2.840.10008.1.1	Verification SOP Class
    %% 49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,49

    %% 1.2.276.0.7230010.3.0.3.6.4    Our Implementation Class UID
    %%

    <<1,  %% 1 = Message Command information. 0 = Message Data Set information
      0,0,0,0,
      205,
      0,1,0,0,
      65,78,89,45,83,67,80,32,32,32,32,32,32,32,32,32, %% "ANY-SCP         "
      98,98,98,98,98,98,32,32,32,32,32,32,32,32,32,32, %% "bbbbbb          "
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,21,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,51,46,49,46,49,46,49,32, %% "1.2.840.10008.3.1.1.1 "
      0,0,46,1,0,255,0,48,0,0,17,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,49,64,0,0,17,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,50,80,
      0,0,
      58,81,
      0,0,4,0,0,64,0,82,0,0,27,
      49,46,50,46,50,55,54,46,48,46,55,50,51,48,48,49,48,46,51,46,48,46,51,46,54,46,52,85,
      0,0,15,
      79,70,70,73,83,95,68,67,77,84,75,95,51,54,52>>. %% "OFFIS_DCMTK_364"
