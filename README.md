# reconnections

Reconnections is an Erlang/OTP application that provides a uniform
interface for managing connections to external services such as
databases.

## Rationale
Service drivers tend to implement different connection logic: some
don't handle reconnections at all, some reconnect but crash if the
service isn't available on startup, etc. Covering all possible scenarios usually requires implementing ad hoc
logic on every project. This library attempts to abstract driver
specifics and allow to setup reconnection strategies via configuration.

Some of the design goals are:

- Provide a uniform API across different libraries.
- Provide out of the box drivers for the most commonly used Erlang libraries
  (e.g. epgsql, eredis, etc.).
- Make it easy to add new drivers.
- Start connection attempts when the application starts, without
  making them a hard depdendency, in other words: don't crash if a
  connection can't be established on application startup.
- Provide or simplify the implementation of circuit breakers to avoid
  requesting disconnected services.

The aim is to help implementing systems as those described in
[Stacking Theory for Systems Design](https://medium.com/@jlouis666/stacking-theory-for-systems-design-2450e6300689) by
Jesper L. Andersen and
[It's About the Guarantees](https://ferd.ca/it-s-about-the-guarantees.html) by Fred Hebert.

## Development status

This is still in an experimental stage; we are currently studying the
behavior of different libraries so we can later flesh out the proper
API to manage the connections.

## Build

    $ rebar3 compile

## Notes on library error behaviors

Behaviour of each service when there are no connection.

### eRedis

- On start: If the redis server is not available, eredis fails and doesn't try to reconnect.
- Disconnection: If there is a disconnection after the connection was correctly established, eredis keeps on trying to reconnect to the server.

### ePgsql

- On start: If the postgresql server is not available, epgsql fails and doesn't try to reconnect.
- Disconnection: If there is a disconnection after the connection was correctly established, epgsql sends an exit signal to the process that started the connection.

### CQerl

In _eredis_ the sleep time between attempts to reconnect can be set with the fifth paremeter `ReconnectSleep` in the `eredis:start_link/5` call.
