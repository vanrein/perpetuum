#!/usr/bin/env python
#
# perpetuum-codegen.py -- Given a Petri net, generate driver code for it.
#
# This is a code generator from a set of Petri nets with suitable labels.
# The output consists of:
#  - gen tables of places and transitions, modelled after <perpetuum/model.h>
#  - use a minimum perfect hash to offer searches by key
#  - gen initialisation and, for singletons, instance information w/ marking
#  - setup initial countdown for places (TODO: incorporate inhibiting arcs)
#  - process multiplicity inscriptions on arcs by generating sequences of arcs
#  - invocations of actions as declared by transition labels
#  - processing of events as declared by transition labels
#  - an implementation in an Erlang module based on behaviour(gen_perpetuum)
#  - dropped: starting/ending of activities as declared by place labels
#  - dropped: processing of activity output as declared by place labels
#
# Next to the C generator, there now also is one for Erlang.  The reason
# being that the match between Erlang and Petri nets is almost magical:
# Unbounded integers allow us to efficiently check the constraints of
# transitions, and message-passing with pattern matching makes it really
# simple to pickup external events that (attempt to) drive transitions.
# The output is a .erl module file that loads a behaviour(gen_perpetuum)
# for its generics, much like a gen_server, except that its backcalls
# are handle_transition -- whose return values are used to update the
# markings of the Petri net in self().
#
# From: Rick van Rein <rick@openfortress.nl>


import os
import sys
import string
import random

from math import log, ceil

# Try to make demo code generation (notably cmph) as stable as possible
random.seed (65537)

import pntools.petrinet
import cmph


#
# Load the Petri net to work on
#

if len (sys.argv) not in [2,3]:
	sys.stderr.write ('Usage: %s infile.pnml [outdir]\n' % sys.argv [0])
	sys.stderr.write ('TODO: We currently process only one PNML file at a time\n')
	sys.exit (1)

progname = sys.argv [1]
progbase = os.path.splitext (progname) [0]

if len (sys.argv) >= 3:
	outdir = sys.argv [2] + os.sep
else:
	outdir = (os.path.dirname (progname) or os.curdir) + os.sep

netin_fn  = progbase + os.extsep + 'pnml'
netout_fn = outdir + os.path.splitext (os.path.basename (progname)) [0] + os.extsep + 'out'
net = pntools.petrinet.parse_pnml_file (netin_fn)
if len (net) != 1:
	sys.stderr.write ('Parsing failed, found %d Petri nets instead of 1\n' % len (net))
	sys.exit (1)
net = net [0]  #TODO# Some day, support multiple Petri nets
pntools.petrinet.write_pnml_file (net, netout_fn)
place_num = len (net.places     )
trans_num = len (net.transitions)
print 'Network name is', net.name
print 'Number of places is', place_num
print 'Number of transitions is', trans_num

#TODO# Combine Petri nets [0..] with matching labels of places and transitions


#
# Map network name to file name and structure name; produce file names
#
neat_net_name = ''.join ( [ c
			for c in net.name.replace ('-', '_').replace (' ', '_')
			if c in  '_' + string.ascii_letters + string.digits
			] )
if not neat_net_name [:1] in string.ascii_letters:
	neat_net_name = 'x' + neat_net_name
if neat_net_name == '':
	neat_net_name = 'perpetuum'
c_fn    = outdir + neat_net_name + '.c'
h_fn    = outdir + neat_net_name + '.h'
erl_fn  = outdir + neat_net_name + '.erl'
# hrl_fn  = outdir + neat_net_name + '.hrl'
pkey_fn = outdir + neat_net_name + '.pkey'
tkey_fn = outdir + neat_net_name + '.tkey'
pidx_fn = outdir + neat_net_name + '.pidx'
tidx_fn = outdir + neat_net_name + '.tidx'
ptyp_fn = outdir + 'petritypes.h'
# dot_fn  = outdir + neat_net_name + '.dot'
# png_fn  = outdir + neat_net_name + '.dot'
pnml_fn = outdir + neat_net_name + '.pnml'

#
# Write out the PNML file -- the composed result of the set of Petri nets
#
#TODO:COMPOSED#pnml = snakes.pnml.dumps (net)
#TODO:COMPOSED#print 'Produced PNML, size', len (pnml)
#TODO:COMPOSED#print 'Writing PNML to', pnml_fn
#TODO:COMPOSED#pout = open (pnml_fn, 'w')
#TODO:COMPOSED#pout.write (pnml)
#TODO:COMPOSED#pout.close ()
#TODO:COMPOSED#print 'Written', pnml_fn


#
# Map places and transitions each to their own indices
#

places = net.places     .keys ()
transs = net.transitions.keys ()

range2type = [
	( 1<< 8, 'uint8_t'  ),
	( 1<<16, 'uint16_t' ),
	( 1<<32, 'uint32_t' ),
	( 1<<64, 'uint64_t' )
]

placeref_t = None
for (r,t) in range2type:
	if len (places) < r:
		placeref_t = t
		break

transref_t = None
for (r,t) in range2type:
	if len (transs) < r:
		transref_t = t
		break

if placeref_t is None or transref_t is None:
	sys.stderr.write ('FATAL: Unable to fit places and transitions into 64 bit integers\n')
	sys.exit (1)


place_mph = cmph.generate_hash (places, algorithm='bdz')
trans_mph = cmph.generate_hash (transs, algorithm='bdz')

place_mph.save (pidx_fn)
trans_mph.save (tidx_fn)

place_idx = { }
trans_idx = { }
place_list = [''] * place_num
trans_list = [''] * trans_num

for p in places:
	place_idx [p] = place_mph.lookup (p)
	place_list [place_idx [p]] = p
for t in transs:
	trans_idx [t] = trans_mph.lookup (t)
	trans_list [trans_idx [t]] = t

open (pkey_fn, 'w').write ('\n'.join (place_list) + '\n')
open (tkey_fn, 'w').write ('\n'.join (trans_list) + '\n')

#DEBUG# print 'Places:', place_idx
#DEBUG# print 'Transitions:', trans_idx

#UNUSED# rp,wp = os.pipe ()	#PROBLEM# Limited pipe buffer
#UNUSED# place_mph.save (rp)
#UNUSED# place_hashtable = wp.read (65537)
#UNUSED# ...generate code for insertion...
#UNUSED# 
#UNUSED# rp,wp = os.pipe ()	#PROBLEM# Limited pipe buffer
#UNUSED# trans_mph.save (rp)
#UNUSED# trans_hashtable = wp.read (65537)
#UNUSED# ...generate code for insertion...


#
# Construct tables
#

# Generate typedefs for transref_t and placeref_t
tout = open (ptyp_fn, 'w')
tout.write ('''/* Petri Net storage types for ''' + neat_net_name + ''' */

typedef ''' + transref_t + ''' transref_t;
typedef ''' + placeref_t + ''' placeref_t;

''')
tout.close ()

# Generate headers for the .h and .c files
cout = open (c_fn, 'w')
hout = open (h_fn, 'w')
hout.write ('/* ' + neat_net_name + '''.h
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


''')

# Generate the header for the .erl file
eout = open (erl_fn, 'w')
eout.write ('% ' + neat_net_name + '''.erl
%
% This is a generated file.  Do not edit it, but rather its source and
% run perpetuum to produce a new version of this file.
%
% Please file issues with https://github.com/vanrein/perpetuum/issues
%
% With compliments from the ARPA2.net / InternetWide.org project!
%

-module( \'''' + neat_net_name + '''\' ).
-behaviour( gen_perpetuum ).

-export([
	start_link/3,
	transit/0,
	places/0,
	initial_placebits/0,
	initial_marking/1,
	transmap/1,
	sentinel/1
]).


''')


# Generate #defines for each of the transition names (referenced in events)
hout.write ('/* Index numbers for transitions, by TRANS_INDEX_name */\n')
for tr in range (len (trans_list)):
	hout.write ('#define TRANS_INDEX_' + trans_list [tr] + ' ' + str (tr+1) + '\n')
hout.write ('\n')


cout.write ('/* ' + neat_net_name + '''.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "''' + neat_net_name + '''.h"


#ifdef PETRINET_WITHOUT_NAMES
#define NAME_COMMA(x)
#else
#define NAME_COMMA(x) x,
#endif


''')

# Transition names are atoms in Erlang, so we do not need to generate them


# Generate the lists of places and transitions from each of their neighbours
def genlist (kind, dict, name, reflist):
	cout.write ('static const ' + kind + 'ref_t ' + name + ' [] = { ' + str (len (reflist)))
	for ref in reflist:
		i = 1 + dict [ref]
		cout.write (', ' + str (i))
	cout.write (' };\n')
p2tm = [ (e.source,e.target,int(e.inscription)) for e in net.edges if net.places.has_key (e.source) and e.type != 'inhibitor' ]
t2pm = [ (e.source,e.target,int(e.inscription)) for e in net.edges if net.places.has_key (e.target) ]
p2t = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.source) and e.type != 'inhibitor' for multi in range (int (e.inscription)) ]
p2i = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.source) and e.type == 'inhibitor' ]
t2p = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.target) for multi in range (int (e.inscription)) ]
for p in place_list:
	genlist ('trans', trans_idx, p + '_trans_out',     [ t for (p2,t) in p2t if p==p2 ] )
	genlist ('trans', trans_idx, p + '_trans_out_inh', [ t for t in trans_list if (p,t) in p2i ] )
	cout.write ('\n')
for t in trans_list:
	genlist ('place', place_idx, t + '_place_in',      [ p for (p,t2) in p2t if t==t2 ] )
	genlist ('place', place_idx, t + '_place_out',     [ p for (t2,p) in t2p if t==t2 ] )
	cout.write ('\n')

# Generate the place_topo_t[] from the array of each place's inputs and outputs
cout.write ('static const place_topo_t ' + neat_net_name + '_places [] = {\n')
for p in place_list:
	cout.write ('\t{ NAME_COMMA ("' + p + '") ')
	cout.write (p + '_trans_out, ')
	cout.write (p + '_trans_out_inh },\n')
cout.write ('};\n\n')


# For Erlang, generate the transit() and places() listing functions
eout.write ('% Transitions in their standard order of occurrence\n%\n')
eout.write ('transit() -> [')
comma = ' '
for tr in trans_list:
	#TODO# Map transition name to erlang atom
	eout.write (comma + "'" + tr + "'")
	comma = ', '
eout.write (' ].\n\n')
eout.write ('% Places in their standard order of occurrence\n%\n')
eout.write ('places() -> [')
comma = ' '
for plc in place_list:
	#TODO# Map place name to erlang atom
	eout.write (comma + "'" + plc + "'")
	comma = ', '
eout.write (' ].\n\n\n')


# For Erlang, determine the maximum delta to a place.
#
# This is important, as it defines the range for detecting under/overflow.
# This explains why we take incoming arcs as well as outgoing into account.
# Furthermore, the initial marking should be supported for each place.
#
# Inhibitor arcs are excluded because they are generated in a different way.
#
place2range = {}
for p in places:
	place2range [p] = max (1,net.places [p].marking)
for (p,t,m) in p2tm:
	if m > place2range [p]:
		place2range [p] = m
for (t,p,m) in t2pm:
	if m > place2range [p]:
		place2range [p] = m
vector_bits = 0
max_placebits = 0
place2bits = {}
for p in places:
	# Store as a pair: (range-bits, left-shift)
	place2bits [p] = (int (ceil (log (place2range [p], 2))), vector_bits)
	# One sentinel bit and enough bits to hold this place's range
	vector_bits += 1 + place2bits [p][0]
	if place2bits [p][0] > max_placebits:
		max_placebits = place2bits [p][0]

# Now determine the number of bits used in the Erlang virtual machine
def erlang_next_placebits (cur_placebits, intsz=None):
	if intsz is None:
		intsz = (cur_placebits + 2) * place_num + 1
	if intsz < 60:
		intsz = 60
	elif  intsz < 3*64:
		intsz = 3*64
	else:
		intsz = (intsz + 63) & -64
	next_placebits = (intsz - 1) / place_num - 1
	assert (next_placebits > cur_placebits)
	return next_placebits
needed_placebits = erlang_next_placebits (max_placebits-1)
eout.write ('% Initial bitfield vector is 60 or N*64 bits long, N>=3, with\n')
eout.write ('% ' + str (vector_bits) + ' bits for all of the ' + str (place_num) + ' places,\n')
eout.write ('% and the sign bit to spare for possible future use with timeouts.\n')
eout.write ('\n\n')

# Given a place number and the number of placebits, set a place's count
def erlang_place_value (place_idx, placebits, value):
	assert (0 <= value < (1 << placebits))
	return value << ((placebits + 1) * place_idx)

# Construct a inhibitor word for a given place given the placebits
def erlang_inhibit_sentinel (place_idx, placebits):
	inhibitor_value = (1 << placebits) - 1
	return erlang_place_value (place_idx, placebits, inhibitor_value)

# Construct the sentinel bits for a given value of placebits
def erlang_sentinel (placebits):
	retval = 0
	onebit = 1 << placebits
	for i in range (place_num):
		retval = (retval << (placebits + 1)) | onebit
	return retval

# Code to produce the initial number of placebits, used when setting
# up a new instance of this Petri Net.
#
eout.write ('% Return the initial place bits for this Petri Net.\n')
eout.write ('%\n')
eout.write ('initial_placebits() -> %d.\n' % needed_placebits)
eout.write ('\n\n')

# Code to produce the initial marking for only the smallest sentinel,
# as this is before any growth spur has occurred.
#
eout.write ('% Return the initial marking for this Petri Net,\n')
eout.write ('% again parameterised with the number of place bits.\n')
eout.write ('%\n')
inimark = 0
for pi in range (place_num):
	plcmark = net.places [place_list [pi]].marking
	inimark |= erlang_place_value (pi, needed_placebits, plcmark)
#UNUSED@ eout.write ('initial_marking(  0 ) -> initial_marking( %d );\n' % needed_placebits)
eout.write ('initial_marking( %d ) -> %s.\n' % (needed_placebits,inimark))
eout.write ('\n\n')

# Code to produce the smallest transmap outputs as literals,
# with endless expansion in a dynamic last clause.
#
eout.write ('% Return a transmap beyond N bit integers; initially, N=%d.\n')
eout.write ('% The transmap is only read and usually remains literal;\n')
eout.write ('% Only very large Petri Nets need to scale up dynamically.\n')
eout.write ('%\n')
curbits = needed_placebits
for i in range (5):
	eout.write ('transmap(%5d ) -> #{\n' % (curbits))
	t_unique = {}
	t_unique_num = 0
	addends = [ 0 ] * trans_num
	subbers = [ 0 ] * trans_num
	sentinels = [ erlang_sentinel (curbits) ] * trans_num
	for (p,t,m) in p2tm:
		pi = places.index (p)
		ti = transs.index (t)
		subbers [ti] += erlang_place_value (pi, curbits, m)
	for (t,p,m) in t2pm:
		pi = places.index (p)
		ti = transs.index (t)
		addends [ti] += erlang_place_value (pi, curbits, m)
	for (p,t) in p2i:
		pi = places.index (p)
		ti = transs.index (t)
		sentinels [ti] |= erlang_inhibit_sentinel (pi, curbits)
	for ti in range (trans_num):
		# Collect the initial values for this transition
		transname = trans_list [ti]
		# Construct the output for this transition (str(i) is without 'L')
		comma = ',' if ti < trans_num - 1 else ''
		eout.write ('\t\t\t\'%s\' => { %s, %s, %s }%s\n' % ( transname, str(addends [ti]), str(subbers [ti]), str(sentinels [ti]), comma ) )
	eout.write ('\t};\n')
	lastcurbits = curbits
	curbits = erlang_next_placebits (curbits)
# Final clause: 
#UNUSED# eout.write ('transmap(    0 ) ->\n\t\ttransmap( %d );\n' % needed_placebits)
eout.write ('transmap(    N ) when N div 64 == 0 ->\n\t\tgen_perpetuum:reflow_transmap( transmap( %d ),N ).\n' % lastcurbits)
eout.write ('\n')

# Code to produce the smallest sentinels as literals, with endless
# expansion in a dynamic last clause.
#
eout.write ('% Return the sentinel value for a Petri Net of given\n')
eout.write ('% number of place bits.  Again, mostly literal.\n')
eout.write ('%\n')
curbits = needed_placebits
for i in range (5):
	eout.write ('sentinel(%5d ) -> %d;\n' % (curbits,erlang_sentinel (curbits)))
	lastcurbits = curbits
	curbits = erlang_next_placebits (curbits)
#UNUSED# eout.write ('sentinel(    0 ) -> sentinel( %d );\n' % needed_placebits)
eout.write ('sentinel(    N ) -> gen_perpetuum:reflow_sentinel( sentinel( %d ),N ).\n' % lastcurbits)
eout.write ('\n\n')


# Generate the bootstrapping code for a new process instance
# that implements this Petri Net in Erlang.
#
eout.write ('% Construct a new process running ' + neat_net_name + ' instances\n')
eout.write ('% as specialisations of the gen_perpetuum behaviour.\n')
eout.write ('%\n')
eout.write ('% Provided parameters are the standard {M,F,A} form for callbacks,\n')
eout.write ('% used for processing transitions under application logic.\n')
eout.write ('%\n')
eout.write ('start_link( CallbackMod,CallbackFun,CallbackArg ) ->\n')
eout.write ('\tInitArgs = { self(),?MODULE,{ CallbackMod,CallbackFun,CallbackArg } },\n')
eout.write ('\tproc_lib:start( gen_perpetuum,init,InitArgs ).\n')
eout.write ('\n\n')


# Generate function prototypes for all the transitions' actions

hout.write ('/* Function prototypes for transition actions */\n')
for t in trans_list:
	hout.write ('trans_retcode_t trans_action_' + t + ' (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);\n')
hout.write ('\n')

# Generate the trans_topo_t[] from the array of each place's inputs and outputs
cout.write ('static const trans_topo_t ' + neat_net_name + '_transitions [] = {\n')
for t in trans_list:
	cout.write ('\t{ NAME_COMMA ("' + t + '") ')
	cout.write (t + '_place_in, ')
	cout.write (t + '_place_out, ')
	cout.write ('trans_action_' + t + ' ')
	cout.write ('},\n')
cout.write ('};\n\n')

# Generate init vectors for places, and optional singleton array
# For Erlang, the initial marking is a bigint representing a vector
hout.write ('/* Place initialisation */\n')
#OLD# eout.write ('% Initial marking\n%\n')
#OLD# sentinel_imark = -1 << (bigint_bits - 1)
#OLD# sentinel_shift = 0
for plc in place_list:
	ini_mark = net.places [plc].marking
	hout.write ('#define PLACE_INIT_' + plc + ' { ' + str (ini_mark) + ' }\n')
	#OLD# sentinel_imark = sentinel_imark + (int (ini_mark) << place_bits)
	#OLD# sentinel_shift = sentinel_shift + (place_bits + 1)
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static place_t the_' + neat_net_name + '_places [] = {\n')
for plc in place_list:
	cout.write ('\tPLACE_INIT_' + plc + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')
#OLD# eout.write ('% The initial marking for this Petri net (with sentinel bits)\n%\n')
#OLD# eout.write ('initial_marking() -> ' + str (sentinel_imark) + '.\n\n\n')

# Generate init vectors for transitions, and optional singleton array
#TODO:FROMHERE#
# For Erlang, generate the dynamic transition-indexed record
hout.write ('/* Place initialisation; countdown := empty inputs + non-empty inhibitors */\n')
#OLD# eout.write ('% Transition records; describing the initial offsets of in/out transitions\n')
for tr in trans_list:
	# initial "countdown" is zero normal plus non-zero inhibitors trans
	ini_countdown = ( str (len ( [ src for (src,tgt) in p2t
	                               if tgt == tr
	                               and net.places [src].marking == 0 ] ) )
	                + ' + '
			+ str (len ( [ src for (src,tgt) in p2i
	                               if tgt == tr
	                               and net.places [src].marking >  0 ] ) ) )
	hout.write ('#define TRANS_INIT_' + tr + ' { ' + str (ini_countdown) + ', 0, 0 }\n')
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static trans_t the_' + neat_net_name + '_transitions [] = {\n')
for tr in trans_list:
	cout.write ('\tTRANS_INIT_' + tr + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')
eout.write ('\n\n')

# Generate optional code for global variables, which simplifies embedded code
#TODO# Gen topology, gen transitions, gen places
hout.write ('#ifdef PETRINET_SINGLETONS\n')
hout.write ('#ifdef PETRINET_GLOBAL_NAME\n')
hout.write ('extern petrinet_t PETRINET_GLOBAL_NAME;\n')
hout.write ('#else\n')
hout.write ('extern petrinet_t the_' + neat_net_name + ';\n')
hout.write ('#endif\n')
hout.write ('#endif\n\n')
cout.write ('''#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
petrinet_t PETRINET_GLOBAL_NAME = {
#else
petrinet_t the_''' + neat_net_name + ''' = {
#endif
	NAME_COMMA (.colour = "the_''' + neat_net_name + '''")
	.topology = {
		/* Topology is inlined due to PETRINET_SINGLETONS */
		NAME_COMMA (.name = "''' + neat_net_name + '''")
		.place_num = ''' + str (place_num) + ''',
		.trans_num = ''' + str (trans_num) + ''',
		.place_ary = &''' + neat_net_name + '''_places [-1],
		.trans_ary = &''' + neat_net_name + '''_transitions [-1],
		/* TODO: Support for inital USRDEF_PETRINET_FIELDS */
	},
	.place_ary = &the_''' + neat_net_name + '''_places [-1],
	.trans_ary = &the_''' + neat_net_name + '''_transitions [-1],
	/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */
	/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */
	/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */
#ifndef PETRINET_GLOBAL_NAME
};
#else
};
#endif
#endif

''')

# Generate the petrinet_topo_t with the static structure, based on the arrays
hout.write ('''#ifndef PETRINET_SINGLETONS
extern const petrinet_topo_t ''' + neat_net_name + ''';
#else
#ifdef PETRINET_GLOBAL_NAME
#define ''' + neat_net_name + ''' (&PETRINET_GLOBAL_NAME.topology)
#else
#define ''' + neat_net_name + ''' (&the_''' + neat_net_name + '''->topology)
#endif
#endif

''')

cout.write ('''#ifndef PETRINET_SINGLETONS
const petrinet_topo_t ''' + neat_net_name + ''' = {
	NAME_COMMA (.name = "''' + net.name + '''")
	.place_num = ''' + str (place_num) + ''',
	.trans_num = ''' + str (trans_num) + ''',
	.place_ary = &''' + neat_net_name + '''_places [-1],
	.trans_ary = &''' + neat_net_name + '''_transitions [-1],
};
#endif

''')

# Print an end remark
hout.write ('\n\n/* End of generated file ' + neat_net_name + '.h */\n')
cout.write ('\n\n/* End of generated file ' + neat_net_name + '.c */\n')
eout.write ('\n\n%% End of generated file ' + neat_net_name + '.erl\n')

# Close output files
hout.close ()
cout.close ()
eout.close ()

