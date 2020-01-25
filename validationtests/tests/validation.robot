*** Settings ***
Documentation  Wolfpacs should correctly handle a c-echo request
Library  ../libraries/EchoSCU.py


*** Variables ***
${host}     localhost
${port}     11112

*** Test Cases ***
Validation Using Implicit Little
    ${assoc}=    Get implicit assoc   ${host}  ${port}
    Is Established  ${assoc}
    Send C Echo     ${assoc}
    Release         ${assoc}

Validation Using Explicit Little
    ${assoc}=    Get explicit assoc   ${host}  ${port}
    Is Established  ${assoc}
    Send C Echo     ${assoc}
    Release         ${assoc}

Validation Using Explicit Big
    ${assoc}=    Get explicit big assoc   ${host}  ${port}
    Is Established  ${assoc}
    Send C Echo     ${assoc}
    Release         ${assoc}
