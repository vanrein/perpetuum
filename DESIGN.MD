# Scheduling Dynamic Processes with Petri Nets

<img alt="Perpetuum Mobile" src="pix/240px-PerpetuumMobile.gif" style="float: right;"/>

>   *Perpetuum is a Petri-net scheduler generator.  This model can be useful
>   when dealing with asynchronous or event-based activity.  Especially in
>   situations where either end users desires vary, or where the process may
>   develop over time, it is very useful to be able to manage the process from
>   a well-known process formalism, guided by graphical tools.*

**WARNING:** *This article was an early investigation document.  It may not be entirely
up to date, but was kept as a reference to the background of this project.*

Perpetuum was designed to accommodate the
[Key Manager](http://internetwide.org/blog/2016/12/20/idhub-2-middleware.html)
in the ARPA2.net component
[IdentityHub](http://internetwide.org/blog/2016/06/24/iwo-phases.html),
which is supposed to coordinate many tool or protocol specific
details; this situation made it ideal to generate structures automatically
&mdash; and flawlessly.


# Our Petri Nets

There are many variations of Petri nets; our use of the theory is rather
modest:

-   P/T Petri nets; the identity passing through is an orthogonal factor
    to identify the net, but these are never mixed so it is improper to speak
    of Coloured Petri Nets.

-   They have inhibitor arcs, which is a necessity to unleash full
    computational potential in Petri nets.

-   They may have multiplicity annotated on arcs, in which case they unfold to
    multiple arcs in parallel.  Parallel arcs are independent, but they will
    be neighbours to simplify implementation.


## Execution Semantics

-   We maintain counters for each *instance* of a P/T Petri Net separately,
    and don't distinguish the tokens.  We do maintain data for the instance,
    to be manipulated by the various transitions.

-   Each instance's place holds a counter for the number of tokens presently
    stored, and when it is updated we sometimes update transitions about their
    change of state.  The values are initialised to the Petri net's initial
    marking.

-   Transitions have a `countdown` counter that indicates how many of the
    incoming arcs should report a state change before the transition may
    proceed.  The initial value is the number of non-empty inhibitor arcs
    plus the number of input arcs that are unable to fill the transition.

-   Each arc represents the consumption or production of exactly one token.
    This simplifies the logic of passing on state changes, *almost* to the
    level that we only need to report changes from 0 to 1 and back.

-   For inhibitor arcs, the precise meaning is indeed that changes from 0 to
    positive and from positive to 0 need to be reported to inhibited
    transitions.

-   For input arcs to a transition, a sequence of one or more as immediate
    neighbours indicates multiplicity.  When the number of tokens rises from
    *N* to *N+M*, the same arcs numbered (from 0) as *<N..N+M]* are activated;
    likewise, dropping from *N+M* to *N* deactivates arcs numbered (from 0) as
    *<N..N+M]*.

-   As a special case of the former, when going from 0 to 1 tokens in a place,
    one outgoing arc is activated; and the reverse in the opposite direction.
    When going from 1 to 2, the second multiple-parallel arc is activated
    where possible.

-   When we say that a place's output arc activates, it means that it does its
    work to enable the transition to which it is an input; effectively, this
    means lowering the transition's `countdown` timer (or increase it in the
    reverse direction).  For an inhibitor arc, the logic reverses.

-   Transitions whose `countdown` is 0 can fire; that does not mean that they
    will fire immediately.  Recursive scheduling is about doing that, but the
    flat scheduler makes a pass after having made any such changes to any
    number of places.  The flat scheduler also assumes that new transitions may
    have their `countdown` dropped to 0 after at least one transition has
    fired.

-   Single-threaded execution of Petri net scheduling is the only realistic
    option; otherwise, locks are needed to manage the Petri net state, and this
    could easily lead to deadlocks, livelocks and perhaps starvation.  Even in
    a proven-correct Petri net!  We think of a Petri net as a (more detailed)
    scheduler that is similar to an event loop, and that starts up other work
    which may be run in worker threads.

-   When a transition has fired, the flat scheduler knows that it needs to go
    through an entire round; for reasons of fairness, as well as to implement
    the unpredictable nature that users may expect from their Petri nets, it is
    possible to start a full loop (checking up on all transitions) from a new
    and totally random point after a transition has fired.  (This is however an
    optional mechanism; other than fairness there is no real reason that the
    semantics enforce this; the non-determinism in Petri net semantics can
    very well be implemented fully deterministically.)


## Petri Net Compiler

-   A place knows all its successor transitions, and can place a token in them.

-   A place does not need to know its predecessors.

-   A transition knows its successor places, to which it can offer tokens.

-   A transition would need to know its preceding places if it needs to claim
    locks on tokens; this is only required in the recursive scheduling model,
    but not with the flat scheduler.

-   The relations implied by arcs can be stored in static tables holding
    numeric indexes pointing to their peers.

-   The token counter for each place are stored in a table somewhere; this is
    the current marking for an *instance* of a Petri net.

-   The `countdown` of each transition is also stored in a data table under an
    *instance* ofa Petri net.

-   Initial `countdown` value in a transition is the number of places with
    insufficient tokens (that is, arc multiplicity minus initial marking,
    but not lower than zero) plus the number of inhibiting arcs whose places
    have a zero initial marking.

-   The compiler produces static tables for the Petri net topology, and a
    fixed-size structure for dynamic data tables.  Dynamic data is per
    Petri net instance, and such instances can point to the related static
    data.

-   This structure enables generic routines that work on a pointer to a
    Petri net instance and any additional information it needs in the form of
    *indexes of* places, transitions to work on.

-   Only very little code is needed; we need to attempt firing a transition,
    and we need a (flat) scheduler.

-   Given the statically known number of transitions and places, we can pick
    the shortest possible integer format.  There are more ways to compress
    the tables generated, see [WHEN-SIZE-MATTERS.MD](WHEN-SIZE-MATTERS.MD).

-   Transitions need to connect to callbacks.  Each returns whether the
    transition fired, needed a backoff time, and so on.

-   We will also need to be open to system events, such as network traffic
    arriving at the Petri net instance.  Such events may fire a transition
    as long as control resides outisde of the scheduler (due to the
    single-threading assumption) and can simply verify that `countdown` is
    zero.


## Transactions and Journaling

-   Since we generate code, it may be possible to establish durability for the
    Petri nets, that is making them survive across restarts of the
    Perpetuum-generated code.

-   While storing data but not processing events, we may miss updates.  This
    is especially true for responses to previously sent requests.  In such
    cases, it is very likely that a timeout would be triggered.  This means
    that running processes may break off as failed.  Correct behaviour in terms
    of the Petri net semantics, but not necessarily useful.

-   Storage can be orthogonally achieved between instances of Petri nets.
    Petri nets that fall back to their initial markin may be taken out of
    storage when they also support silent creation.  This can help to compress
    data storage.

-   Transitions would represent transactional changes to Petri nets.  Their
    successful firing can be stored before we update the various incoming and
    outgoing places.  Side effects in the environment cannot be rolled back
    by their inherent nature.

-   A journal for a Petri net instance consists of an initial marking
    (possibly *the* initial marking) followed by a sequence of transitions
    that have successfully fired, including their timing of firing.  It is
    possible to re-fire these transitions with temporary overrides for the
    actions in the transitions -- where the overrides exist to enforce the
    success of the firing action.  The override may in fact be part of the
    normal flow of events, and it may indeed be helpful to resend requests
    that were sent in a prior run?

-   It looks like it is advisable to have idempotent actions, rather than
    ones that complain "you already asked for that" which is of no real use
    when a Petri net controls that this is not normally asked.  Being able
    to repeat actions means that replay during journal recovery may be
    useful.  For now, that's a thought without proper design backing it.


## Monitoring

-   Given that we generate our code, it should not prove difficult to
    facilitate monitoring by also generating SNMP interfacing code.

-   It would be relatively straightforward to indicate how many tokens have
    been "stuck" in a given place, and for how long.

-   To this end, we can have a field with the time of initial or last token
    injection into an instance of a Petri net place.

-   It would also be possible to keep track of the firing rate of transitions.

-   Note that the values from various Petri net instances must be combined
    when reporting about Petri nets over SNMP.

-   Note that parallel processes might make this difficult; but our assumption
    of single-threadedness seems to overrule that concern.

 
