#include <stdlib.h>
#include <stdio.h>

#include "development_net.h"

int main (int argc, char *argv []) {
	flat_schedule_run (PARMARG (&the_development_net));
	int pr;
	for (pr=1; pr<=50; pr++) {
	// for (pr=50; pr>=1; pr--) {
		printf ("Injecting a token into place %d\n", pr);
		inject_tokens (PARMARG_COMMA (&the_development_net) pr, 1);
		flat_schedule_run (PARMARG (&the_development_net));
	}
	exit (1);
}
