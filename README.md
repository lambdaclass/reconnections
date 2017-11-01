reconnections
=====

An OTP application

Build
-----

    $ rebar3 compile

Error behaviour
---------------

Behaviour of each service when there are no connection.

| Services | Initialization with no connection | Lose connection                | Request when no connection                   |
| -------- | --------------------------------- | ------------------------------ | -------------------------------------------- |
| Postgres | Send EXIT signal, no reconnect    | Send EXIT signal, no reconnect | noproc _(message to a non existent process)_ |
| Redis    | Send EXIT signal, no reconnect    | Try to reconnect               | {connection_error, Reason}                   |

In _Redis_ the sleep time between attempts to reconnect can be set with the fifth paremeter `ReconnectSleep` in the `eredis:start_link/5` call.
