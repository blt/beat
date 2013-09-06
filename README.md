# beat - A small, textual TCP chat server.

[![Build Status](https://travis-ci.org/blt/beat.png)](https://travis-ci.org/blt/beat)

The aim of this project is pedagogy. In particular, it's meant to provide a base
for intermediate Erlang developers at the Rackspace Hackday a chance to work on
and dissect an OTP project. We'll try to build a interesting chat server in a
day, from a basic skeleton.

## Quick Start

To run `beat` ensure that you have [`relx`](https://github.com/erlware/relx)
installed on your system and do the following:

```
> make && relx
...
> _rel/bin/beat console
```
Now connect to localhost port 27182 and enjoy the sweet flow of integers.
