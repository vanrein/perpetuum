# Minimal Code from Petri Nets

<img alt="Perpetuum Mobile" src="image/240px-PerpetuumMobile.gif" style="float: right;"/>

> *In embedded environments, every byte counts.  Though Perpetuum can easily
> build a capable and scalable environment that can handle tens of thousands
> of instances and possibly many different sets of Petri Nets, it is also
> quite good at producing tiny code, if you ask nicely.*

Microcontrollers can have as little as a few kB of memory, quite different
from modern-day desktops.  But Petri Nets are useful for all interactive
programs, and that includes microcontrollers as much as GUIs and
network-interactive software.  So how to generate small systems?

## Drop name strings

Configuration option: `PETRINET_WITHOUT_NAMES=yes`

When you draw a Petri net, you assign names to its places and transitions,
or your editor does it for you.  These names can be useful in all sorts of
debugging output, but they also require storage space; first, for the strings
themselves, and second for pointers to them.

If you don't need this information, you can get rid of it with this flag.

The Petri net compiler shuffles the order of places, as well as the order
of transitions.  This however, is not a result *pure* evil; first off, the
ordering of labels might not go as you would expect anyway (alphabetical
rather than numerical) but more importantly, we prepare a minimal perfect
hash on the names.  Even if you should choose to not use that, you might
like by-products based on it, that could help you lookup mappings in the
build environment.  (FWIW, no such tool has been made.  Who needs it?)

You might wonder if you can trace back from code you are debugging to the
original design.  Usually you can, because the static variables generated
from the Petri net are labelled with this same name; so during source-level
debugging you may still see these snippets of information; their storage
is in the debugging section of the program, which is habitually removed
by `strip` and `install` programs which should already be part of your
control flow while getting the program loaded into your microcontroller.


## Use the smallest reference types

It is a standard facility of the Perpetuum code generator to minimise the
size of the references to transitions and places, as they are used a lot in
the description of the topology for the composed set of Petri Nets.

The types `transref_t` and `placeref_t` are used throughout the include file
`<perpetuum/model.h>` and in the remainder of the code.  These types are set
to the smallest unsigned integer type available that can hold the desires
number of values.

The code generator will not go to extremes; it will choose from the standard
integer types `uint8_t`, `uint16_t`, `uint32_t` and `uint64_t` from include
file `<stdint.h>`.

Your average embedded system is likely to have less than 255 places and less
than 255 transitions, meaning that each' references can be encoded in one byte.
The storage size for the topology roughly equals the double of the number of
arcs (doubly because references are mostly needed in both directions).


## Ensure that your Petri Net is k-bounded

TODO: Not implemented yet: `tokenctr_t` of various sizes

When you know that your set of Petri Nets is *k*-bounded, meaning that no
place will ever go beyond *k* tokens, you can shrink the size of the
`tokenctr_t` stored for each place.  Moreover, you could bring it in line
with the mathematically optimal integer size for an embedded CPU, which may
find 8 bit or 16 bit calculations much simpler than its 32 bit versions.

Petri Net tooling can derive if your net is *k*-bounded, and there are
design tricks to help getting to this property.  If you choose *k* such that
it fits in the desired integer length (so *k=255* for 8 bits, or *k=65535*
for 16 bits) you can reduce the size of `tokenctr_t` accordingly.

The routines that modify the `tokenctr_t` are aware of both the old and
new values, and whether the value should be raised or lowered.  In
debugging mode, assertion statements are available that trigger an
error when these conditions are not properly met.  Keep in mind though,
that such runtime tests are only useful for the run at hand; it will not
nearly give you the assurance that a *k*-bound analysis has to offer.


## Build single-threaded code

Configuration option: `BUILD_SINGLE_THREADED=yes`

Code is much simpler if it can forego locking schemes, and even simpler
if it need not worry about lock-free operations either.  If your system
can work in an asynchronous fashion, it can be single-threaded and that
greatly simpifies all of your code.

Currently, you don't really have a choice.  We do not support threading
inside the Petri Net scheduler at all.  If we would, we would either end
up with a system that could deadlock (if we used full locks) or starve
or livelock (if we only attempted to lock).  This would even be possible
in systems that had been proven to be free of such situations; it would
be the result of the competition among the transitions over input tokens.

Usually, any threading is best done by starting a worker when the Petri Net
invokes an action; feedback from workers can be brought back in as events
that trigger transactions in the Petri Net.


## Use the Flat Scheduler, not the Recursive Scheduler

Configuration option: `PETRINET_FLAT_SCHEDULING=yes`

Configuration option: `PETRINET_RECURSIVE_SCHEDULING=no`

There are principally two scheduling mechanisms in Perpetuum, and the
default is the best for embedded systems.  The flat scheduler passes
over transitions in a sort of main loop, and so its use of the stack
is rather limited.  Contrast that with the recursive scheduler, which
can make nested calls to handle something new that came up; so injecting
a token may end up triggering a transition, which adds tokens elsewhere,
which triggers another transition, and so on.  In a recursive scheduler,
these causal relations lead to nested calls, and so heavy and somewhat
unpredictable stack use.  This is generally undesirable in embedded
environments.

It is possible to use either of the schedulers, or both.  By default,
scheduling is flat, and flat only.

At present, the recursive scheduler is not available at all.  When we add
it, *if* we add it, it will be off by default.  Consider recursive
scheduling only in very high-speed systems.  (By the time we actually
turn to it, we may prefer something queue-based though.  No promise that
we will actually implement this scheduler at all!)


## Specify Singletons when you can

Configuration option: `PETRINET_SINGLETONS=yes`

By default, the assumption is that a set of Petri Nets can have any
number of instances.  In the smallest systems, this may not be the case.

By specifying that (all) Petri Net sets are singletons, the code will be
built slightly differently; rather than a reference from each instance to
the topology and other generic information, a single static variable is
created for the single instances, named `the_` plus the type name.  Instead
of referencing the topology and generic information, it inlines it into the
instance data; this saves pointer traverals and also increases chances of
automated processing.

If your program generates code for more than one set of Petri Nets, beware
that this option applies to all of them; so don't use it when one or more
of your sets are not used as singletons.  In an embedded system, this is
usually not a problem.


## Reference Global Variables

Configuration option: `PETRINET_GLOBAL_NAME=myPetriNetName`

When you have only one set of Petri Nets, and only one instance of it,
you can skip passing the parameter that references the Petri Net
instance.  Instead, the code can refer to a global variable.

This setting implies `PETRINET_SINGLETON=yes`, but it is stronger.
It is an error to specify a `PETRINET_GLOBAL_NAME` but not
`PETRINET_SINGLETONS=yes`.

Where there could be multiple singletons derived from multiple sets of
Petri Nets causing the need for the parameter, this option actually
states that the code always addresses the same singleton.  Although it
would be uncommon, it is still possible to generate separate code chunks
for separate sets of Petri Nets and use this option for each of the
chunks of code.  Beware when you trod that path though, you might end up
in prickly bushes.

The point of this configuration option is not just to save a parameter
to often-called functions.  The point is to allow the compiler to make
many more inferences about where data is stored, especially in combination
with our vigorous use of the `const` keyword and, hopefully, the `static`
keyword.  This should save a lot of memory lookups, resulting in (much)
faster code.


## Static Initialisation

Somewhat dependent on your embedded environment, it may or may not be
advantageous to create an initial marking through C's `static` variable
initialisation.  To do this, you should set the tokens in your Petri net
Editor, though perhaps not in simulation mode but in editing mode.

When you save your Petri net as PNML, the initial marking would normally
be written out as part of it.  Perpetuum recognises this, and when it
generates places it will add the tokens to the diagram.  Although a
function `inject_tokens()` exists, this should not be considered a user
function to call; it is only for internal use.  Interaction with users
is limited to actions and events.


## Measurements

TODO: Create a few simple applications with these flags, see how well we do!

The size of the Petri Net standard routines is rather small.  Compiled with
`-Os` on an `x86_64` architecture, we have:

  * `runtime.o` occupies 397 bytes in the `.text` segment
  * `flatsched.o` occupies 131 bytes in the `.text` segment
  * ARM code tends to be twice as large

These routines are therefore very, very small and can be included in all but
the very smallest of microcontrollers.  Note: This is without setting any
of the options above!

TODO: Set the options, see what happens.

TODO: Also measure data size, it *does* matter &mdash; a lot!


## There may be more...

We have not done it yet, but:

  * We could make one large array with all the `placeref_t` and `transref_t` and point in there with an index, assuming this would be smaller than an address, and work just as fast
  * We might look for overlap in the `placeref_t` and `transref_t` arrays

For now, we are open to patches of this kind, but have no serious plans
of doing this work within this project.  Reason being, we want to be supportive
of embedded applications, but lack the time/energy to go that far.
