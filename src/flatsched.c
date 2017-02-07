/* flatsched.c -- Flat Scheduler, so without needing Recursion, for Petri nets.
 *
 * This scheduler should run whenever tokens have been injected into a Place,
 * and it will continue to run until all actions have been handled, or all is
 * placed on hold.  In the latter case, it will return a timer delay.
 *
 * This is one of two scheduling approaches; they may be used together or as
 * alternatives.  The flat scheduler requires a constant, small amount of
 * stack but needs to run through the list of transitions before it knows if
 * any work remains to be done.  The recursive scheduler on the other hand,
 * starts work as soon as it is encountered, so a token insertion can lead to
 * a ripple of activity through the Petri net.  This however, requires a
 * recursive approach and so a potentially large stack.  The recursive
 * scheduler is an option for server and desktop machines, but not for most
 * embedded architectures.  Combinations are possible; for instance, recursion
 * might some day be constrained to a certain maximum depth, and then flat
 * scheduling can take over as an "outside layer" to pickup any pieces left.
 *
 * The scheduler is *always* a single-threaded system.  There is no expected
 * gain, and many expected losses, from having threads compete on a Petri net's
 * transition firing.  It is much more interesting and useful to use actions
 * called during transitions to initiate worker threads and processes.
 *
 * What is supported, is injection of tokens from threads that are not the
 * scheduler's thread.  This is useful because these other threads may be
 * signaling back on work started by the Petri net, but in another thread or
 * process.  The injection of tokens has been implemented as a lock-free
 * concurrent mechanism.
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#include <stdint.h>
#include <stdbool.h>

#include <time.h>

#include <perpetuum/model.h>


/* Run through the given Petri Net, only stopping when nothing is left to
 * be done.  This flat scheduler is relatively simple, which is great for
 * embedded applications, but probably less so for timing-critial tools
 * based on large Petri Nets, as the system will end with a full cycle
 * through the transitions before giving up.  It may also need to go through
 * almost a full cycle to get from one transition to the next.  We may need
 * to invent a smarter scheduler sometime later, and this may not even be
 * the recursive scheduler.
 *
 * The value returned suggest the next wakeup time.  It is ~(time_t)0 if
 * there was no timer to wait for.
 */
time_t flat_schedule_run (petrinet_colour_t *pcn) {
	transref_t fired_last = 1;
	transref_t tr = 0;
	time_t wakeup = ~ (time_t) 0;
	while (true) {
		//
		// The loop "infinitely" loops from last to first transition
		if (tr == 0) {
			tr = TOPO (pcn)->trans_num;
		}
		//
		// If the transition [tr] can fire, make it happen
		if (REF2TRANS (pcn, tr).countdown == 0) {
			time_t now = time (NULL);
			time_t nbf = REF2TRANS (pcn, tr).notbefore;
			if (nbf <= now) {
				if (try_firing (pcn, tr)) {
					fired_last = tr;
					wakeup = ~ (time_t) 0;
					// At least one full looping to come
					goto proceed;
				}
			} else if (nbf <= wakeup) {
				wakeup = nbf;
			}
		}
		//
		// Stop looping when nothing has changed for a full round
		if (tr == fired_last) {
			return wakeup;
		}
		//
		// Proceed to the next transition to consider
proceed:
		tr--;
	}
}

