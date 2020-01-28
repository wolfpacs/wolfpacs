*** Settings ***
Documentation  Wolfpacs should correctly handle a c-echo request
Library  ../libraries/storagescu.py


*** Variables ***
${host}     localhost
${port}     11112
${image}    ${CURDIR}/CT-MONO2-16-brain.dcm

*** Test Cases ***
Validation Using Implicit Little
    ${assoc}=    Send ctimage implicit little  ${host}  ${port}
    Is Established  ${assoc}
    Send Image      ${assoc}  ${image}
    Release         ${assoc}

Validation Using Explicit Little
    ${assoc}=    Send ctimage explicit little  ${host}  ${port}
    Is Established  ${assoc}
    Send Image      ${assoc}  ${image}
    Release         ${assoc}

Validation Using Explicit Big
    ${assoc}=    Send ctimage explicit big  ${host}  ${port}
    Is Established  ${assoc}
    Send Image      ${assoc}  ${image}
    Release         ${assoc}
