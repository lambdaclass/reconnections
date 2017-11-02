reconnections
=====

An OTP application

Build
-----

    $ rebar3 compile

Error behaviour
---------------

Behaviour of each service when there are no connection.

| Services | Initialization without connection | Loses connection               | Request when no connection                   |
| -------- | --------------------------------- | ------------------------------ | -------------------------------------------- |
| epgsql   | Send EXIT signal, no reconnect    | Send EXIT signal, no reconnect | noproc _(message to a non existent process)_ |
| eredis   | Send EXIT signal, no reconnect    | Try to reconnect               | {connection_error, Reason}                   |

In _eredis_ the sleep time between attempts to reconnect can be set with the fifth paremeter `ReconnectSleep` in the `eredis:start_link/5` call.
