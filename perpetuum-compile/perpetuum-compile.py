#!/usr/bin/env python
#
# perpetuum-compile.py -- Given a Petri net, generate driver code for it.
#
# Yeah I know, it's lame to call it petrification if it is meant to provide
# flexibility in process driving.  TODO: Call it perpetuum-cogen?
#
# This is a code generator from a set of Petri nets with suitable labels.
# The output consists of:
#  - tables of places and transitions, modelled after <perpetuum/model.h>
#  - uses a minimum perfect hash to offer searches by key
#  - TODO: invocations of actions as declared by transition labels
#  - TODO: processing of events as declared by transition labels
#  - TODO: starting/ending of activities as declared by place labels
#  - TODO: processing of activity output as declared by place labels
#
# From: Rick van Rein <rick@openfortress.nl>


import os
import sys
import random

from snakes.nets import PetriNet, Place, Transition, StateGraph, Variable
import snakes.plugins

import cmph


#TODO#
#TODO# Start of Petri net generation for development purposes, semi-random
#TODO#

prng = random.Random ()
prng.seed (1234567890)
one_in_ = 10
place_num = 50
trans_num = 40
place_out = 'place%02d'
trans_out = 'trans%02d'

net = PetriNet ('development_net')
for p in range (place_num):
	net.add_place (Place (place_out % p))
for t in range (trans_num):
	net.add_transition (Transition (trans_out % t))
for p in range (place_num):
	for t in range (trans_num):
		if prng.uniform (0, one_in_) < 1:
			net.add_input  (place_out % p, trans_out % t, Variable('TODO'))
		if prng.uniform (0, one_in_) < 1:
			net.add_output (place_out % p, trans_out % t, Variable('TODO'))


#TODO# End of Petri net generation for development purposes, semi-random


#TODO# Combine Petri nets based on matching labels of places and transitions

#
# Map network name to file name and structure name
#
neat_net_name = net.name.replace (' ', '_').replace ('-', '_')  #TODO#  Escape/subst etc
net_name = 'petrinet_' + neat_net_name


#
# Output a GraphViz .dot file with the graph format
#
snakes.plugins.load ('gv', 'snakes.nets', 'nets')
#TODO:NOTFOUND# net.draw (',' + neat_net_name + '.png')
s = StateGraph (net)
s.build ()
#TODO:NOTFOUND# s.draw (',' + neat_net_name + '.png')

#
# Map places and transitions each to their own indices
#

places = [ p.name for p in net.place      () ]
transs = [ t.name for t in net.transition () ]

place_mph = cmph.generate_hash (places, algorithm='bdz')
trans_mph = cmph.generate_hash (transs, algorithm='bdz')

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

#DEBUG# print 'Places:', place_idx
#DEBUG# print 'Transitions:', trans_idx

#TODO# rp,wp = os.pipe ()	#TODO# Limited pipe buffer
#TODO# place_mph.save (rp)
#TODO# place_hashtable = wp.read (65537)
#TODO# 
#TODO# rp,wp = os.pipe ()	#TODO# Limited pipe buffer
#TODO# trans_mph.save (rp)
#TODO# trans_hashtable = wp.read (65537)


#
# Construct tables
#

cout = open ('/tmp/' + neat_net_name + '.c', 'w')
hout = open ('/tmp/' + neat_net_name + '.h', 'w')

# Generate headers for the .h and .c files
hout.write ('/* ' + neat_net_name + '''.h
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with TODO
 *
 * With compliments from the ARPA2.net /InternetWide.org project!
 */


#include <perpetuum/model.h>


''')


cout.write ('/* ' + neat_net_name + '''.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with TODO
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "''' + neat_net_name + '''.h"


''')


# Generate the lists of places and transitions from each of their neighbours
def genlist (kind, dict, name, reflist):
	cout.write ('static ' + kind + 'ref_t ' + name + ' [] = { ' + str (len (reflist)))
	for ref in reflist:
		i = 1 + dict [ref]
		cout.write (', ' + str (i))
	cout.write (' };\n')
p2t = [ ]
t2p = [ ]
for t in trans_list:
	for p in net.transition (t).input ():
		p2t.append ( (p[0].name,t) )
	for p in net.transition (t).output ():
		t2p.append ( (t,p[0].name) )
for p in place_list:
	genlist ('trans', trans_idx, p + '_trans_out',     [ t for t in trans_list if (p,t) in p2t ] )
	genlist ('trans', trans_idx, p + '_trans_out_inh', []) #TODO#
	cout.write ('\n')
for t in trans_list:
	genlist ('place', place_idx, t + '_place_in',      [ p for p in place_list if (p,t) in p2t ] )
	genlist ('place', place_idx, t + '_place_in_inh',  []) #TODO#
	genlist ('place', place_idx, t + '_place_out',     [ p for p in place_list if (t,p) in t2p ] )
	cout.write ('\n')

# Generate the place_t[] based on the array of each place's inputs and outputs
cout.write ('static place_t ' + neat_net_name + '_places [] = {\n')
for p in place_list:
	cout.write ('\t{ "' + p + '", ')
	cout.write (p + '_trans_out, ')
	cout.write (p + '_trans_out_inh },\n')
cout.write ('};\n\n')

# Generate the trans_t[] based on the array of each place's inputs and outputs
cout.write ('static trans_t ' + neat_net_name + '_transitions [] = {\n')
for t in trans_list:
	cout.write ('\t{ "' + t + '", ')
	cout.write (t + '_place_in, ')
	cout.write (t + '_place_in_inh, ')
	cout.write (t + '_place_out },\n')
cout.write ('};\n\n')

# Generate the petrinet_t with the static structure, based on the arrays of trans_t and place_t
hout.write ('extern petrinet_t ' + net_name + ';\n\n')
cout.write ('petrinet_t ' + neat_net_name + ''' = {
	"''' + net.name + '''",
	''' + str (place_num) + ', ' + str (trans_num) + ''',
	''' + neat_net_name + '''_places, ''' + neat_net_name + '''_transitions
};
''')

# Print an end remark
hout.write ('\n\n/* End of generated file ' + neat_net_name + '.h */\n')
cout.write ('\n\n/* End of generated file ' + neat_net_name + '.c */\n')

#TODO-TMP# print 'Net ::', dir (net)
#TODO-TMP# print 'Transition ::', dir (net.transition (trans_list [0]))
#TODO-TMP# print 'Transition Input::', net.transition (trans_list [0]).input ()
#TODO-TMP# print 'Transition Output::', net.transition (trans_list [0]).output ()
#TODO-TMP# print 'Place ::', dir (net.place (place_list [0]))
