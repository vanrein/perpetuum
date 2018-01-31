/* This is an "interactive" test command.
 *
 * Use q|Q to quit, d|D to signal dawn, s|S to signal sunset.  Press enter!
 *
 * The result will run through the Petri net, signal states between runs,
 * and can be made to stop at yellow between dawn and sunset events.  Note
 * how any active run of red..green...yellow continues to yellow when dawn
 * has been entered, just as the Petri net prescribes.
 *
 * Sorry for the horrible interaction, it's just a demo...
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#include <stdlib.h>
#include <stdio.h>

#include <fcntl.h>
#include <errno.h>

#include "traffic_light_nightly.h"

#include <perpetuum/api.h>


trans_retcode_t trans_action_stop (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	if (random () & 0x00000001) {
		printf ("Stop\n");
		return TRANS_SUCCESS;
	} else {
		return TRANS_FAILURE;
	}
}

trans_retcode_t trans_action_go (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	if (random () & 0x00000001) {
		printf ("Go\n");
		return TRANS_SUCCESS;
	} else {
		return TRANS_FAILURE;
	}
}

trans_retcode_t trans_action_caution (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	if (random () & 0x00000001) {
		printf ("Caution\n");
		return TRANS_SUCCESS;
	} else {
		return TRANS_FAILURE;
	}
}

transref_t trlist_dawn [] = { 1, TRANS_INDEX_dawn };

trans_retcode_t trans_action_dawn (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	if (opt_evdata != NULL) {
		printf ("Event-triggered Dawn\n");
		return TRANS_SUCCESS;
	} else {
		return TRANS_FAILURE;
	}
}

transref_t trlist_sunset [] = { 1, TRANS_INDEX_sunset };

trans_retcode_t trans_action_sunset (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	if (opt_evdata != NULL) {
		printf ("Event-triggered Sunset\n");
		return TRANS_SUCCESS;
	} else {
		return TRANS_FAILURE;
	}
}




int main (int argc, char *argv []) {
	// flat_schedule_run (PARMARG (&the_traffic_light_nightly));
	int pr;
	int quit = 0;
	char ch;
	int flags = fcntl (0, F_GETFL);
	if (flags < 0) {
		perror ("Failed to fcntl (F_GETFL)\n");
		exit (1);
	}
	flags |= O_NONBLOCK;
	if (fcntl (0, F_SETFL, flags) < 0) {
		perror ("Failed to fcntl (F_SETFL)\n");
		exit (1);
	}
	printf ("Enter q|Q to quit, d|D to signal dawn, and s|S to signal sunset...\n");	
	printf ("Prior to scheduler with %s=%d, %s=%d, %s=%d\n",
			the_traffic_light_nightly.topology.place_ary [1].name,
			the_traffic_light_nightly.         place_ary [1].available,
			the_traffic_light_nightly.topology.place_ary [2].name,
			the_traffic_light_nightly.         place_ary [2].available,
			the_traffic_light_nightly.topology.place_ary [3].name,
			the_traffic_light_nightly.         place_ary [3].available);
	sleep (3);
	fflush (stdout);
	while (!quit) {
	// int i = 10;
	// while (i-- > 0) {
	// for (pr=1; pr<=3; pr++) {
	// for (pr=3; pr>=1; pr--) {
		// printf ("Injecting a token into place %d\n", pr);
		// inject_tokens (PARMARG_COMMA (&the_traffic_light_nightly) pr, 1);
		printf ("Entering scheduler with %s=%d, %s=%d, %s=%d, %s=%d\n",
				REF2PLACE_TOPO(&the_traffic_light_nightly,1).name,
				REF2PLACE     (&the_traffic_light_nightly,1).available,
				REF2PLACE_TOPO(&the_traffic_light_nightly,2).name,
				REF2PLACE     (&the_traffic_light_nightly,2).available,
				REF2PLACE_TOPO(&the_traffic_light_nightly,3).name,
				REF2PLACE     (&the_traffic_light_nightly,3).available,
				REF2PLACE_TOPO(&the_traffic_light_nightly,4).name,
				REF2PLACE     (&the_traffic_light_nightly,4).available);
		fflush (stdout);
		flat_schedule_run (PARMARG (&the_traffic_light_nightly));
		sleep (1);
		while (read (0, &ch, 1) == 1) {
			switch (ch) {
			case 'd':
			case 'D':
				printf ("Processing dawn event\n");
				process_event (PARMARG_COMMA (&the_traffic_light_nightly) trlist_dawn, "DAWN-non-NULL");
				break;
			case 's':
			case 'S':
				printf ("Processing sunset event\n");
				process_event (PARMARG_COMMA (&the_traffic_light_nightly) trlist_sunset, "SUNSET-non-NULL");
				break;
			case 'q':
			case 'Q':
				quit = 1;
				break;
			default:
				break;
			}
		}
	}
	exit (0);
}
