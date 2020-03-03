-define(VERIFICATION, <<"1.2.840.10008.1.1">>).
-define(SECONDARY_CAPTURE, <<"1.2.840.10008.5.1.4.1.1.7">>).
-define(CT_IMAGE_STORAGE, <<"1.2.840.10008.5.1.4.1.1.2">>).
-define(MR_IMAGE_STORAGE, <<"1.2.840.10008.5.1.4.1.1.4">>).
-define(PRINT_JOB, <<"1.2.840.10008.5.1.1.14">>).

%%
-define(FINDStudyRootQueryRetrieveInformationModel, <<"1.2.840.10008.5.1.4.1.2.2.1">>).
-define(MOVEStudyRootQueryRetrieveInformationModel, <<"1.2.840.10008.5.1.4.1.2.2.2">>).
-define(GETStudyRootQueryRetrieveInformationModel, <<"1.2.840.10008.5.1.4.1.2.2.3">>).

%% findscu -W -k "(0040,0100)[0].Modality=CT" localhost 11112
-define(FINDModalityWorklistInformationModel, <<"1.2.840.10008.5.1.4.31">>).
