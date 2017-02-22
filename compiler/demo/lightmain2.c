#include <stdlib.h>
#include <stdio.h>

#include "traffic_light_nightly.h"

#include <perpetuum/api.h>


trans_retcode_t trans_action_stop (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	printf ("Stop\n");
	return TRANS_SUCCESS;
}

trans_retcode_t trans_action_go (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	printf ("Go\n");
	return TRANS_SUCCESS;
}

trans_retcode_t trans_action_caution (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	printf ("Caution\n");
	return TRANS_SUCCESS;
}

trans_retcode_t trans_action_dawn (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	printf ("Dawn\n");
	return TRANS_SUCCESS;
}

trans_retcode_t trans_action_sunset (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata) {
	printf ("Sunset\n");
	return TRANS_SUCCESS;
}



int main (int argc, char *argv []) {
	// flat_schedule_run (PARMARG (&the_traffic_light_nightly));
	int pr;
	printf ("Prior to scheduler with %s=%d, %s=%d, %s=%d\n",
			the_traffic_light_nightly.topology.place_ary [1].name,
			the_traffic_light_nightly.         place_ary [1].available,
			the_traffic_light_nightly.topology.place_ary [2].name,
			the_traffic_light_nightly.         place_ary [2].available,
			the_traffic_light_nightly.topology.place_ary [3].name,
			the_traffic_light_nightly.         place_ary [3].available);
	fflush (stdout);
	int i = 10;
	while (i-- > 0) {
	// for (pr=1; pr<=3; pr++) {
	// for (pr=3; pr>=1; pr--) {
		// printf ("Injecting a token into place %d\n", pr);
		// inject_tokens (PARMARG_COMMA (&the_traffic_light_nightly) pr, 1);
		printf ("Entering scheduler with %s=%d, %s=%d, %s=%d\n",
				REF2PLACE_TOPO(&the_traffic_light_nightly,1).name,
				REF2PLACE     (&the_traffic_light_nightly,1).available,
				REF2PLACE_TOPO(&the_traffic_light_nightly,2).name,
				REF2PLACE     (&the_traffic_light_nightly,2).available,
				REF2PLACE_TOPO(&the_traffic_light_nightly,3).name,
				REF2PLACE     (&the_traffic_light_nightly,3).available);
		fflush (stdout);
		flat_schedule_run (PARMARG (&the_traffic_light_nightly));
	}
	exit (0);
}
