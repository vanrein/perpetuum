/* runtime.c -- Running around in circles, forever and ever (or until done).
 *
 * These routines operate on the structures of <perpetuum/model.h> to make
 * them process events.  To that end, you may inject_tokens() and have the
 * new additions passed around until the Petri net returns to rest.
 *
 * It is up to an event loop to inject new tokens, and it is up to a library
 * of actions to cause all sorts of activity to be initiated.  Note the
 * distinction between an action (a snappy function call that quickly
 * returns whether it succeeded in sparking off whatever was desired) and
 * activities (which, conformant to UML lingo, takes time to come from
 * start to end).  Perpatuum facilitates the activity concept through a
 * couple of separate actions with intermediate state being stored.  This
 * state could be something like an LDAP request identifier, used to
 * recognised a response as paired with a request.
 *
 * The model processing logic in this file only concerns itself with the
 * quick actions, and leaves the more advanced handling of activities to
 * surrounding libraries.  Note that Perpetuum logic follows Petri nets
 * in having no notion of atomic, long-lived actions.  They would cause
 * locking behaviour and all the complexity that comes with it.
 *
 * The code below is designed to be part of the event loop.  Specifically,
 * it is not re-entrant; the Perpetuum model intends to distribute work
 * as threads or processes, but thanks to quick actions it does not need
 * to be concurrent internally; this is a strength or weakness, dependent
 * on your view on life... perhaps atomic operations can be used to ease
 * this choice in later revisions.
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <time.h>

#include <perpetuum/model.h>


/* The following code is single-threaded, and probably should remain like that.
 * When multiple threads try to initiate Petri net transitions at the same
 * time, their locks will interfere in non-trivial ways.  When hard locking,
 * expect deadlock to occur in a properly validated Petri-net.  When trying
 * to lock, expect livelocks in the same, validated Petri-nets.
 */
#ifndef BUILD_SINGLE_THREADED
#  error "We only guarantee proper functioning for single-threaded builds!"
#  include "opa_primitives.h"
#endif


/* Assertion statements with IMPLIES read well.
 */
#define IMPLIES(a,b) ((~(a))||(b))


/* Inject new tokens.  The return value indicates if this made any change
 * of interest, which basically means if it could be useful to pass through
 * the transitions (in case of PETRINET_FLAT_SCHEDULING).
 *
 * The addend supplied may be positive or negative.  Zero is meaningless.
 *
 * TODO: Consider allowing global variable referencing if there's just one.
 *
 * TODO: Token injection is_WILL_BE_TODO lock-free concurrent.  That enables
 * independent worker threads and processes to signal completion of a task,
 * including reception of a response to a previously sent request or the
 * completion of a slow I/O operation.
 */
bool inject_tokens (PARMDEF_COMMA(pnc) placeref_t pr, int addend) {
	bool done_sth = false;
	transref_t tr, tri;
	//
	// Get hold of the old and new counter of available tokens
	tokenctr_t old = REF2PLACE (pnc,pr).available;
	tokenctr_t new = old + addend;
	assert (IMPLIES (addend > 0, new > old));
	assert (IMPLIES (addend < 0, new < old));
	REF2PLACE (pnc,pr).available = new;  //TODO:ALREADY?//
	//
	// Before doing anything else, inhibit any transitions if needed
	if ((old == 0) && (new > 0)) {
		FOR_TRANSREF (tri, tr, REF2PLACE_TOPO (pnc,pr).trans_out_inh) {
			REF2TRANS (pnc,tr).countdown++;
		}
	}
#ifdef PETRINET_RECURSIVE_SCHEDULING
	//
	// Before actually updating following transitions, try to activate
	// them directly, without causing the up-then-down changes to others
#warning "TODO: Recursive scheduling is not yet implemented -- skipping it"
#endif
	//
	// Now update transitions if our new number of tokens is interesting
	// Assume that multiple transitions are stored in immediate succession
	int ctr = 0;
	transref_t prev = NIL_TRANS;
	FOR_TRANSREF (tri, tr, REF2PLACE_TOPO (pnc,pr).trans_out) {
		if (tr == prev) {
			ctr++;
		} else {
			prev = tr;
			ctr = 1;
		}
		if ((old < ctr) && (ctr <= new)) {
			REF2TRANS (pnc,tr).countdown--;
			done_sth = true;
		}
	}
	//
	// We may now relax any inhibitor arcs whose inhibition was removed
	if ((old > 0) && (new == 0)) {
		FOR_TRANSREF (tri, tr, REF2PLACE_TOPO (pnc,pr).trans_out_inh) {
			REF2TRANS (pnc,tr).countdown--;
		}
	}
	//
	// Finally, return whether we have done something of interest
	return done_sth;
}


/* Attempt to fire the given transition.  The return value indicates if
 * this was a success.
 *
 * It is assumed that the transition's countdown has dropped to 0; it is
 * usually initialised to the number of incoming normal arcs, incremented
 * by inhibitor tokens from incoming inhibitor places, and decremented by
 * a usable token over an incoming normal arc.  Multiple incoming arcs
 * from any given place are handled well when they are sequenced together.
 *
 * Firing means a number of things:
 *  - the countdown must be 0 (asserted)
 *  - the notbefore times must have passed (usually counter-based)
 *  - the action related to the transition is offered its opportunity
 *  - tokens are consumed over input arcs and produces over output arcs
 *  - changes to incoming arcs may change the countdown (or it may stay 0)
 *
 * This operation is called from one thread; usually the flat scheduler.
 *
 * TODO: Consider allowing global variable referencing if there's just one.
 */
bool try_firing (PARMDEF_COMMA(pnc) transref_t tr) {
	//
	// The countdown must have reached 0
	assert (REF2TRANS (pnc,tr).countdown == 0);
	//
	// The notbefore time must have passed
	time_t now = time (NULL);
#ifdef DEBUG
	assert (now >= REF2TRANS (pnc,tr).notbefore);
#endif
	//
	// The transition's action can now fire
	trans_retcode_t rv = REF2TRANS_TOPO (pnc,tr).action (
				&now,
				tr,
				&REF2TRANS_TOPO (pnc,tr),
				&REF2TRANS (pnc,tr));
	if (rv != TRANS_SUCCESS) {
		if (REF2TRANS (pnc,tr).firstfail == 0) {
			REF2TRANS (pnc,tr).firstfail = now;
		}
		if (rv != TRANS_FAILURE) {
			REF2TRANS (pnc,tr).notbefore = now + rv;
		}
		return false;
	}
	//
	// Firing the transaction was successful
	// Now pass around the tokens for our firing
	placeref_t pr, pri;
	FOR_PLACEREF (pri, pr, REF2TRANS_TOPO (pnc,tr).place_in) {
		inject_tokens (PARMARG_COMMA (pnc) pr, -1);
	}
	FOR_PLACEREF (pri, pr, REF2TRANS_TOPO (pnc,tr).place_out) {
		inject_tokens (PARMARG_COMMA (pnc) pr, +1);
	}
	//
	// We succeeded...  so be all cheerful and share the fun
	return true;
}

