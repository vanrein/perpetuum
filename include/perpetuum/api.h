/* perpetuum/api.h -- Internal and external API calls for Perpetuum.
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#ifndef PERPETUUM_API_H
#define PERPETUUM_API_H


#include <stdbool.h>


/* External API calls for the scheduling of Petri nets and events.
 */
time_t flat_schedule_run (PARMDEF (pnc));
bool process_event (PARMDEF_COMMA(pnc) transref_list_t tra, void *evdata);
void reset_transition_timer (PARMDEF_COMMA(pnc) transref_t tr);


/* Internal API calls for the scheduling of Petri nets and events.
 */
bool inject_tokens (PARMDEF_COMMA(pnc) placeref_t pr, int addend);
bool try_firing (PARMDEF_COMMA(pnc) transref_t tr, void *opt_evdata);


#endif /* PERPETUUM_API_H */

