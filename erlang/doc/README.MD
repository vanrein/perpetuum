# Erlang Library Code

This directory contains generic Erlang library components and tests.

To use it, include it as a behaviour:

    -behaviour( gen_perpetuum ).

The library search path must be set in environment variable `ERL_PATH`,
pointing to the `perpetuum-${Perpetuum_VERSION}` directory.  Do not
include the `ebin` subdirectory.

If that doesn't work, then accept our apologies, we may not be quite up to
speed with that part of Erlang.  You can always fall back to including
the packages binary path, so this time including the `ebin` subdirectory,
to the `erlc` command in a `-pa` parameter.

