/* evfire.c -- Fire transitions in response to events.
 *
 * When not in the (flat) scheduler, a program can process events that cause
 * a transition to occur.  Transitions that may fail without the event being
 * triggered.  This code is used to trigger such events.
 *
 * In general, an event can give rise to multiple transitions, but only one
 * will actually be fired per event.  When a transition action reports that
 * it does not fire, then the event can be used to fire another.  This is why
 * the general handling of an event deals with a list of transitions.
 *
 * Events are assumed to have been trimmed down to an instance of a Petri net,
 * because we are not working with colour in our simple P/T Petri nets.
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#include <stdbool.h>

#include <perpetuum/model.h>
#include <perpetuum/api.h>


/* Process the occurrence of an event by attempting to cause a transition to
 * fire.  Multiple transitions can be provided as an alternative.
 *
 * This function must not be called while the scheduler is also active; we
 * assume single-threaded semantics for reasons described elsewhere, and one
 * of its results is that this function should only be called while there is
 * no scheduler activity going on.
 *
 * Note: In general, it is non-deterministic which transition from a set of
 * possible ones will fire.  This is a concept during analysis; we do not
 * currently implement this non-determinism through randomness, but we might
 * have started at a random position in the list of transitions, for example.
 *
 * What we do, is walk through the list from the last entry to the first,
 * and attempt to fire each of the transitions.  As soon as one has fired, we
 * consider the event processed.
 *
 * Although NULL will be properly processed for evdata, it is generally good
 * to provide _something_ because non-NULL evdata helps the trans_action_XXX
 * to distinguish an event-caused call from a scheduler-triggered call.  See
 * the latter as an indication that the transition may take place, and the
 * event as an indication that something interesting has happened.
 *
 * This function returns True when one transition has fired in response to the
 * event, or False when none has fired.  There will never be more than one
 * transition fired in response to the event occurring.
 */
bool process_event (PARMDEF_COMMA(pnc) transref_list_t tra, void *evdata) {
	transref_t tr;
	for (tr = tra [0]; tr > 0; tr--) {
		if (REF2TRANS (pnc,tr).countdown == 0) {
			if (try_firing (PARMARG_COMMA(pnc) tr, evdata)) {
				return 1;
			}
		}
	}
	// When we end here, nothing has been done with the event
	return 0;
}
