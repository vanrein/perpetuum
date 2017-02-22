/* traffic_light_nightly.h
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include <stdint.h>

#include <perpetuum/model.h>


/* Function prototypes for transition actions */
trans_retcode_t trans_action_dawn (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);
trans_retcode_t trans_action_go (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);
trans_retcode_t trans_action_sunset (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);
trans_retcode_t trans_action_caution (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);
trans_retcode_t trans_action_stop (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);

/* Place initialisation */
#define PLACE_INIT_yellow { 1 }
#define PLACE_INIT_red { 0 }
#define PLACE_INIT_night_service { 0 }
#define PLACE_INIT_green { 0 }

/* Place initialisation; countdown := empty inputs + non-empty inhibitors */
#define TRANS_INIT_dawn { 0 + 0, 0, 0 }
#define TRANS_INIT_go { 1 + 0, 0, 0 }
#define TRANS_INIT_sunset { 1 + 0, 0, 0 }
#define TRANS_INIT_caution { 1 + 0, 0, 0 }
#define TRANS_INIT_stop { 0 + 0, 0, 0 }

#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
extern petrinet_t PETRINET_GLOBAL_NAME;
#else
extern petrinet_t the_traffic_light_nightly;
#endif
#endif

#ifndef PETRINET_SINGLETONS
extern const petrinet_topo_t traffic_light_nightly;
#else
#ifdef PETRINET_GLOBAL_NAME
#define traffic_light_nightly (&PETRINET_GLOBAL_NAME.topology)
#else
#define traffic_light_nightly (&the_traffic_light_nightly->topology)
#endif
#endif



/* End of generated file traffic_light_nightly.h */
