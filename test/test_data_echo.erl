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
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,


      16,0,0,21,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,51,46,49,46,49,46,49,32, %% "1.2.840.10008.3.1.1.1 "
      0,0,46,1,0,255,0,48,0,0,17,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,49,64,0,0,17,
      49,46,50,46,56,52,48,46,49,48,48,48,56,46,49,46,50,

      %% 50H = 80 => User Information Item Fields
      80,0,0,58,
      81,0,0,4,
      %% Max length
      0,0,64,0,
      82,0,0,27,
      %% "1.2.276.0.7230010.3.0.3.6.4"
      49,46,50,46,50,55,54,46,48,


      %% "OFFIS_DCMTK_364"
      79,70,70,73,83,95,68,67,77,84,75,95,51,54,52>>.


%% incorrect_associate_ac() ->
%%     <<02, 00, 00, 00, 00, cd,
%%
%%       00, 01,  %% Protocol version
%%       00, 00,  %% Reserved
%%
%%       41, 4e, 59, 2d, 53, 43, 50, 20, 20, 20, 20, 20, 20, 20, 20, 20,
%%       45, 43, 48, 4f, 53, 43, 55, 20, 20, 20, 20, 20, 20, 20, 20, 20,
%%
%%       00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
%%       00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
%%
%%       10, 00, 00, 85,
%%       31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 33, 2e,
%%       31, 2e, 31, 2e, 31,
%%
%%       20, 00, 00, 2e, 01, 00, 00, 00, 30, 00, 00, 11, 31, 2e, 32, 2e,
%%       38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e,
%%
%%       31,
%%
%%       40, 00, 00, 11, 31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30,
%%       38, 2e, 31, 2e, 32,
%%
%%       50, 00, 00, 3a, 51, 00, 00, 04, 00, 01, 00, 00,
%%
%%       52, 00, 00, 1b, 31, 2e, 32, 2e, 32, 37, 36, 2e, 30, 2e, 37, 32,
%%       33, 30, 30, 31, 30, 2e, 33, 2e, 30, 2e, 33, 2e, 36, 2e, 34,
%%
%%
%%       55, 00, 00, 0f, 4f, 46, 46, 49, 53, 5f, 44, 43, 4d, 54, 4b, 5f,
%%       33, 36, 34>>.
%%
alid_associate_ac() ->
    wolfpacs_utils:log_to_binary(
      "02, 00, 00, 00, 00, b8, 00, 01, 00, 00, 41, 4e, 59, 2d, 53, 43,
         50, 20, 20, 20, 20, 20, 20, 20, 20, 20, 45, 43, 48, 4f, 53, 43,
         55, 20, 20, 20, 20, 20, 20, 20, 20, 20,

         00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
         00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,

         10, 00, 00, 15,
         31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 33, 2e,
         31, 2e, 31, 2e, 31,

         21, 00, 00, 19, %% Presentation Contex in A-Associate-AC
         01, 00, 00, 00, %% PrCID, 0, 0, 0
         40, 00, 00, 11, %% Transfer Syntax with length 0x11 -> 17
         31, 2e, 32, 2e, 38, 34, 30, 2e, 31, 30, 30, 30, 38, 2e, 31, 2e,
         32,

         50, 00, 00, 3a, 51, 00, 00, 04, 00, 00, 40, 00,

         52, 00, 00, 1b, 31, 2e, 32, 2e, 32, 37, 36, 2e, 30, 2e, 37, 32,
         33, 30, 30, 31, 30, 2e, 33, 2e, 30, 2e, 33, 2e, 36, 2e, 34,

         55, 00, 00, 0f, 4f, 46, 46, 49, 53, 5f, 44, 43, 4d, 54, 4b, 5f,
         33, 36, 34").
