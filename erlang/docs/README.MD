# Erlang Library Code

This directory contains generic Erlang library components and tests.

To use it, include it as a behaviour:

    -behaviour( gen_perpetuum ).

Might the compiler complain that it cannot find the behaviour, then
be sure to include this directory in the search path:

    erlc -pa /path/to/perpetuum/erlang xyz.erl

