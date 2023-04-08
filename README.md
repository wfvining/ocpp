# OCPP

An implementation of the Open Charge Point Protocol. This is a
prototype implementation of the websocket-based OCPP-j protocol,
version 2.0.1.

# Architectural Notes

## Quality Attributes

### Scalability

The CSMS should be able to handle hundreds of thousands (and
potentially millions of chargers). The White House estimates there are
over 130,000 public chargers deployed in the US as of 2023 as well as
over 3,000,000 vehicles. 1,000,000 managed chargers is a reasonable
target for a nationwide charging network.

On the other hand, the number of gas stations within a single company
seems to be on the order of thousands; this would seem to imply that
even a large charging network would only require management of at most
of thousands of stations. Even under the assumption that there will be
additional quasi-public charging stations (e.g. at apartments or
offices) that increase the number of stations compared to gas
stations—and the assumption that due to the longer charge time more
charging stations will be required than gas stations—a charging
network can realistically only be expected to manage at most tens of
thousands of stations. Even nation wide there are only ~150,000 gas
stations.

However, scalability with respect to concurrent charging sessions and
interactions from customers without incurring high latency is very
important.

### Performance

Low latency. Arbitrarily, my goal is to ensure that 99.999% of
requests receive a response within one second.

Additionally, the system should be expected to experience bursty use
including potentially geographically correlated failures and sudden
influx of restored connections and provisioning requests following
power outages.

Usage under normal conditions may also be bursty with high load in the
mornings and afternoons/evenings.

_I'm not sure that "bursty" is the best word to describe this, but I
can't think of a better one right now._

### Availability

The system must never stop. Requests should time out _rarely_.

Additionally, while it is not a software attribute availability of the
charging stations/EVSE is also very important. unavailable/faulted
equipment should be identified and alerted rapidly. Wherever possible
reboots should be scheduled during low use times. Energy delivery must
not be impeded by the CSMS.

### Security

Important, duh. OCPP has done a substantial amount of work to ensure
the security of the protocol, but there is still room for errors in
the implementation (e.g. weak password hashes, or database
vulnerabilities).

There should be detection and mitigation of denial of service attacks.

### Modifiability

Modifiability must be provided primarily to account for expected
updates and new versions of OCPP. At the other side the architecture
should be designed for modifiability of the core CSMS functionality.

### Interoperability

The CSMS should be interoperable with OCPP 1.6 as well as 2.0.1 and
any future versions. The main route to achieving this will be to
separate the communication/protocol layer (consisting of a state
machine modeling the station & enforcing OCPP requirements) from the
core CSMS functionality which will be accessible via erlang message
passing (i.e. OTP APIs).

Additional interoperability requirements arise for the core CSMS to
interact with arbitrary external services such as such as a
distribution management system or a variety of user & management
systems.
