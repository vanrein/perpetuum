# Perpetuum Test Scripting

<img alt="Perpetuum Mobile" src="../pix/240px-PerpetuumMobile.gif" style="float: right;"/>

> *The test infrastructure, currently geared exclusively at Erlang,
> for the Perpetuum system.  C is bound to follow sometime later.*

Tests are available in `test` under the project directory.

## Using CTest

The builtin tests of the package can be run after configuring with
CMake and building with your local build system.  You can simply
run `ctest` to see the test outputs.  The first few tests are very
slow, and if your interest is with later tests you might want to
speedup with some concurrency, and use `ctest -j 25` instead.

The remainder of this document deals with creating and running 
your own tests, possibly against your own modules.


## Files with Petri Nets

We like to use [GreatSPN](../TOOLS.MD) as an editor and above all as
a simulator for Petri Nets.  It is available for free download and
runs on all the major desktops.

GreatSPN has its own file format named PNPRO.  It is an XML format,
but not compatible with the standard PNML documents.  To give you as
users a choice we settled on the standard, but we do supply the PNPRO
where possible because this wonderful tool won't import PNML files.
We ask you to return the favour when you happen to use the same tool,
as it greatly improves the usability of the workflow.  But don't force
people to use this tool; please always supply PNML as well, like we do
too.


## Files with Test Runs

We coined a simple format for describing test runs, basically as a
single Erlang expression.  The format is a list where two consequtive
elements belong together:

 1. A marking, as a mapping of places to a number of tokens
 2. What can fire, as a list of transition names

The list therefore always has an even number of elements, because
these paired elements were not brought together in a tuple; mostly
to simplify manual entry.

Also as a simplification, you need not specify places with zero tokens.

The first transition in the list is fired at the Petri Net to come
to the next marking and transitions that are armed to fire.

A few oddities of Erlang to be mindful about:

  * End the expression with a dot
  * Comments are prefixed with `%` and not the common `#`
  * Names of places and transitions need not be quoted when styled
    like an identifier in C, but you may use single quotes to get
    more expressive freedom

You should *write your test files manually* because it makes little
sense to test automated output against automated input.  Of course
this is not as important when you *generate a test file from
completely distinct software*.  I can hear you asking, but no, the
GreatSPN simulator has no export facility for this.  We may ask
them, though, they have been willing to help out before.


## Choosing what Tests to Run

We have devised a rather extensive list of tests.  We hope it suits
your needs, but of course it cannot be complete.

All tests run against a compiled Erlang module, the `gen_perpetuum`
behaviour and your `.test` file (although certain tests may ignore the
file or make improper use of it).

### startstop

This test starts a process through your generated module and then tests
if it responds well to a `stop()` invocation.  It will wait for 5 seconds
until your module terminates, and check if it does so with some
dignity.


### syncrun

This test starts a process through your generated module and then passes
through the process laid out in the `.test` file.  The markings will
be checked and so will the list of possible transitions; after this is
done, the first transition will be fired to reach to the next marking
and transition list.

Things tested are:

  * Compare the marking in the test file against the output of
    `marking/1`, where undefined places are set to 0 and the
    resulting markings must match exactly.
  * Ask the Petri Net through `canfire/1` which transitions are
    ready to fire; compare to the list given in the test file.
    (`script`)
  * Iterating over all transitions known to the Petri Net, use
    `canfire/2` to see what transitions can fire, and be sure
    that it contains no more and no less than the list given in
    the test file.  (`others`)
  * Iterating over all transitions known to the Petri Net that
    are *not* ready to fire according to the test file, try to
    fire them at the Petri Net, and harvest the result
    `{retry,marking}` to indicate that it is not currently
    possible.  (`probed`)
  * Invocation of an operation through `event/3`.
  * A `noreply` arranged answer indeed responds with `noreply`.


### asyncrun

This test is very similar to `syncrun`, except that the calls are
made through the asynchronous method `signal/3` and that no
return codes are expected or evaluated.

The reason that we can test this is that introspection functions
such as `marking/1` and `canfire/1,2` are also sent as messages
to the process for the Petri Net instance.  The process may well
accept asynchronous messages, but it handles them internally in
a completely ordered fashion.  So, by the time it gets to handle
the `marking/1` message, it has finished processing our
asynchronous submission through `signal/3`.

The main reason for asynchronicity is to decouple the client
from whatever the Petri Net needs to do in the background.


### applogic

This test is yet another variation of syncrun, but this time
with application logic attached that collects state, and
reportes whether the collected state results in output that
is expected.

This is also the first test against `enquire/2`, a generic
mechanism to request data from the application logic that
lies behind the Petri Net.  These requests are simply
passed in and out by the Petri Net, and are run as part of
its overall process.


### wildsum

This has no purpose for the test script, other than rather
ungratefully seeding a predictable series of seemingly
random values from it.  Once again it is like syncrun,
but it will makes an effort to pass a simple calculation
through the Petri Net, based on its events.  Again, this
uses `enquire/2` to pickup the result.


### syncrun_delayed

As the name indicates, this is yet another variation on
the syncrun test.  It arranges for the transitions to not
fire directly, but rather with a delay.  The application
logic calls for this, as it would do in a real application
where timeouts need to be triggered.  If need be, an
application can return a value that defers an event or
signal to a later time for processing.  This test also
tests the `event/4` call, which allows a maximum timeout
to be specified.  Without this timeout, attempts of
deferral are rejected; with this timeout, there is a
maximum waiting time before attempted deferrals get
rejected.  Note that we are talking of blocked waiting.


### asyncrun_delayed

Similar to syncrun_delayed, except based on the
asynchronous calls, this time testing the `signal/4`
function which adds a timeout.

Unlike syncrun_delayed, there is no blocked waiting
involved here; the signal is sent and control returns.

We used similar trickery to the plain asyncrun to
ensure that we're not sending signals before the
Petri Net is ready for them.  Hadn't we done this,
as we initially didn't by the way, then the Petri Net
will take in signals that match the initial state
but drop anything else.  And it looks really silly
when only certain transition names get through!


### backgrounded

This is somewhat similar to the asyncrun_delayed,
but it is setup differently.  All but the first and
last transitions in the test file are bundled and
sent to the application logic, which arranges for
the transitions to fire in order.  The client has
no idea what is going on until it tries the last
transition.  It cannot check intermediate states,
but the first and last it can check.


### reflow_sentinel

Your generated module holds a function `sentinel/1`
that provides values for the first few numbers of
storage bits per place.  After five or so, it
changes to using a dynamic `reflow_sentinal/4`
operation that starts from the initial Sentinel.
We compare this compiler-supported sequence with
one of our own making.

The other sequence starts with the same initial
Sentinel but then makes a stepwise extension.
It compares the next ten values.  Although this
sequence is also based on the dynamic
`reflow_sentinel/4` operation, it increments in
steps, and therefore even uses a different
computation for the last five in the sequence.

This test does not use the test file, although it
will be read in.  Read about Sentinels in the
[Bit Field Vector documentation](../BITFIELDVEC.MD)
but keep in mind that this test only addresses
the general Sentinel, not the Transition Sentinel.


### reflow_transmap

Your generated module holds a function `transmap/1`
that provides values for the first few numbers of
storage bits per place.  After five or so, it
changes to using a dynamic `reflow_transmap/4`
operation that starts from the initial Transition
Map.  We compare this compiler-supported sequence
with one of our own making.

The other sequence starts with the same initial
Transition Map but then makes a stepwise extension.
It compares the next ten values.  Although this
sequence is also based on the dynamic
`reflow_transmap/4` operation, it increments in
steps, and therefore even uses a different
computation for the last five in the sequence.

This test does not use the test file, although it
will be read in.  Read about Transition Maps in the
[Bit Field Vector documentation](../BITFIELDVEC.MD)
and note that the tests include reflowing of a
Transition Sentinel, unlike the general Sentinel
tested in the reflow_sentinel test.


## Invoking Test Runs

The tool to start a test run is an Erlang script
named `run_tests.erl` which can be started like
any other executable.  As arguments, it expects
a Petri Net name and a sequence of test names
(which are the subsection headers of the
previous section).

The `gen_perpetuum` module is found somewhere
along `$ERL_PATH`; the Erlang module that was
generated for the Petri Net must be translated
to a `.beam` file and will be found by adding
that extension after the Petri Net name and
will be looked for in a number of locations,
usually including the current directory; the
test file is found by adding `.test` after the
Petri Net name and looking for it in a number
of locations, usually including the current
directory.  Test names are looked up in the
internal list, and run one by one.

The output
can be dramatically informative (that's a
euphemism, I suppose) but is easily summarised
in `SUCCESS` or `FAILURE` for each test, as
well as an overall conclusion at the end,
which is also reflected in the usual exit codes
zero for `SUCCESS` and nonzero for `FAILURE`,
which are precisely what CTest expects.

We are aware that `run_tests.erl` is functional,
but not very friendly.  Improvements will be
made, also in an attempt to simplify the build
setup which now requires us to copy all files
into the `test/` subdirectory of the build
directory.  It's a matter of priorities, really.
For now, functionel felt as good enough.


## Erlang Code Checks

There are a few tests that come from the
`erlang` directory underneath the project root.
These are the common tests `ct` and the type
validation test `dialyzer`.  The latter one is
slow, it may take a minute or so on a decent
machine.  No idea why, but it made us choose
to run this test as part of the CTest run
instead of at every build.


## When you find Bugs

Please help us to improve Perpetuum if you found
a bug!  A great way of doing this is described
in the [INSTALL document](../INSTALL.MD).

