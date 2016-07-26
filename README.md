## Simple Cache

This is the simple_cache application from the superb book
[Erlang and OTP in Action](http://www.manning.com/logan/). The full source code
for the book is
[here](https://github.com/erlware/Erlang-and-OTP-in-Action-Source).

All I've done is reformat the code so I can build it with
[rebar](https://bitbucket.org/basho/rebar/wiki/GettingStarted). Credit goes to
the original authors and all mistakes are my own.

Tim

## Build

    $ rebar3 compile

## Running

  * Start 2 nodes in a terminal 'erl –name contact1 -setcookie xxxxxxxx' and
    'erl –name contact2 -setcookie xxxxxxxx'
  * make && ./rebar generate -f
  * Run the application ./rel/simple_cache/bin/simple_cache console

## Useful Links
  * [How to create, build, and run an Erlang OTP application using Rebar](http://skeptomai.com/?p=56#sec-4)
