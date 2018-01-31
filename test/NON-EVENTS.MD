# Non-Event Backcalls; or Pseudo-Transitions

> *Not all backcalls to application logic are made in response to events;
> some backcalls are needed for management purposes.  Here is where we
> describe those.*

The following backcalls or pseudo-transitions are implemented for the
Erlang model; since the C model is designed for manual connection to
event libraries, this would be too detailed there, but it may still
serve as a useful model for implementations in C.

Pseudo-transitions all start with a dollar sign, which means that
their names must be single-quoted in Erlang.  That habit is
reflected below.

## Definition of '$init'

The `'$init'` backcall is used to initialise a new instance
with initial application data.  There is no event data at
this point, since no event is triggering it, but there will
be some information about internal storage passed over,
namely the new Petri Net instance.

Note that instance-specific information can be passed through
the callback arguments, which are passed into every backcall,
including this one.  There is a parameter for `AppState` as
a result of the general backcall mechanism, but it is not
accounted for; do not rely on it.

The return value should be `{noreply,InitialAppState}` and
this `InitialAppState` will be used in the next event
backcall.

## Definition of '$stop'

The `'$stop'` backcall is used when the Petri Net receives
a `stop` message, which is triggered by an invocation of
the `stop/1` function with its process identifier, or
`stop/2` which additionally supplies a reason.

Since this is not triggered by an event, there is no event
data, but instead the call is supplied with
`{Reason,UserMarking}`, and of course with the
last-supplied `AppState`.

The application logic may choose to refuse the request to
stop, by sending back either `{error,Reason}` or
`{retry,Reason}`.  In addition, time may be bought by
sending back `{delay,MilliSecondDelay}` which will
cause a new attempt to be made after the given delay.

To accept the stop request, possibly after cleaning
up the application logic, the responses `{noreply,_}`
and `{reply,_,_}` are both warranted.  The fields
marked with an underscore will be ignored, but are
nonetheless present for good measure towards the
pattern of full transition handlers.

## Definition of '$enquire'

The `'$enquire'` backcall is the outcome of a process
of considering statistical information.  Since such
information consists of Petri Net metadata such as its
current marking and transitions that can fire, both of
which are supplied for in the API, the remaining
concern is to provide application data.  This has been
generalised into this backcall that can server many
other purposes as well.

The naming of this backcall is deliberately distinct
from the SQL misnomer "query", which might be read
as an update mechanism.  Instead, this backcall is
not meant to update application data state, but
just to retrieve values based on it.

Even though this is called explicitly by the user
through the `enquire/2` call, it has no event data
but it will use the field to pass in a query, which
can be any value that makes sense to the application.
Any desired protocol can therefore be shapen here,
including statistics gathering.

The response to this backcall may be `{error,Reason}`
which would be thrown at the calling process; or
it may be `{noreply,_}` which would also be thrown
as an error; or it may succeed with `{reply,Reply,_}`
which would be passed back to the caller as the
return value of the `enquire/2` call.

## Definition of '$default'

The `'$default'` backcall name is used only in the
maps provided to the `gen_perpetuum:trans_switch/4`
callback function.  This function uses a map to
lookup indirect callbacks to make; when a
transition is not found in the map an attempt will
be made to access the default callback according
to the map, by looking up the name `'$default'`
instead.  Note that transitions are mentioned in
the callback mechanism, so second-stage determination
is possible.


## Preview of '$save'

The `'$save'` backcall will probably be used to
request saving application data state to the
application code.

Version information may be supplied as an argument
to this call, so it is unnecessary to supply that
alongside every instance of application data.

## Preview of '$load'

The `'$load'` backcall will probably be used to
request loading back application data state into the
application code, in a form that was previously
generated via the `'$save'` backcall.

Version information will probably be supplied as
an argument to this call, so it is unnecessary to
interpret that from the saved application data;
a bulk mechanism can be used to specify this
information once.

## Preview of '$sync'

The `'$sync'` backcall may be used to synchronise
application data with the application's idea of a
sufficiently stable backing store; be it other nodes
in the network, an in-memory database or disc.

## Preview of '$hibernate'

The `'$hibernate'` backcall will probably be used
to request hibernation from the application logic,
which it might refuse, delay or accept, much like
wiht the `'$stop'` backcall.

## Preview of '$wakeup'

The `'$wakeup'` backwill will probably be used to
indicate to the application logic that the instance
has recovered from a previously accepted
`'$hibernate'` backcall.

