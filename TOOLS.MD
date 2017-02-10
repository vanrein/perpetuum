# Tools to use with Perpetuum

<img alt="Perpetuum Mobile" src="image/240px-PerpetuumMobile.gif" style="float: right;"/>

> *A plethora of Petri-net tools is available, at least in theory.  In
> practice, not all work well.  Usually, the subset of features implemented
> is a less-than-perfect match.*

The feature set required for Perpetuum is quite modest:

  * Standard Place/Transition Nets with only one extension: Inhibitor Arcs
  * We don't really use colour, though we can have many orthogonal instances
    of one Petri net
  * We read Petri nets in the standard
    [PNML notation](http://pnml.org)
  * We process Petri nets with
    [pntools](https://github.com/vanrein/pntools)


## Editors

We tried several, and found most missed certain featuers, which surpised us
in light of our modest requirements.  Currently, it seems that GreatSPN
is the most usable of all.

  * [GreatSPN Editor](http://www.di.unito.it/~amparore/mc4cslta/editor.html)
    is really pleasant to work with, and it can export files in
    [PNML](http://www.pnml.org) -- they actually added it to our request!
    GreatSPN is free for research purposes, and paid for commercial use.
    Its API delivers the best images, mostly thanks to snap-to-grid.
  * [TAPAAL](http://www.tapaal.net) is another tool we found to be useful.
    It focusses strongly on timing on arcs, which we don't use, but that is
    not a problem.  It exports PNML that seems to make our conceptual model
    happy.
  * [PNeditor](http://www.pneditor.org) does not actually enable the use of
    inhibitor arcs, but this is on the road map so we hope it will be usable
    for Perpetuum soonish; its output is encoded in a non-standard manner,
    and it is not clear yet if it will work.  **Note:** You may need to add
    the customary `xmlns` attribute to the `<pnml/>` root element.
  * [WOLFGANG](https://sourceforge.net/projects/wolfgang-editor/)
    works quite nicely, but lacks inhibitor arcs.  This means that only
    very trivial Petri nets for Perpetuum can be edited with it.
  * [CPN Tools](http://cpntools.org) is only kept alive for educational
    purposes and research at the University, according to its authors;
    it barely runs on anything but Windows; it is written in a language
    that is hardly known by anyone.  If anyone wants to "rescue" a useful
    piece of software by moving it over to a language that has stuck
    around, such as Haskell, then this may be a place that needs you!
  * [SNAKES](https://www.ibisc.univ-evry.fr/~fpommereau/SNAKES/API/index.html)
    was our first attempt at loading PNML files into a conceptual model.
    It is not the most direct model we could have, and it does not support
    inhibitor arcs.  We will therefore abandon it for `pntools`.
  * [pntools](https://github.com/irgangla/pntools)
    with
    [our fixes](https://github.com/vanrein/pntools)
    looks like the most processing approach to PNML files.  There is such a
    vast difference in Petri net models that are assumed by tools, that just
    parsing a textual format is the easiest to maintain, even if it may mean
    that we would have to maintain our own fork of this particular tool.

If you tried other tools that we should add, please let us know.  If you
developed tools to surpass what is reported, we would like to hear about it.


## Validation

The ability to validate Petri nets before actually running them is possible
thanks to the domain of
[Model Checking](https://en.wikipedia.org/wiki/Model_checking).
This is mostly an area of academic research on the edge between
computer science and mathematics; its main challenge is that a brute force
approach would require an enormous "state space" to represent all possible
computations, and techniques are needed to provide certainties without
having to resort to that.

There are already
[many tools](https://en.wikipedia.org/wiki/List_of_model_checking_tools)
to help with problems, and Petri nets have attracted many of these because
of their practical uses in so many domains.

The following terms are of interest to the domain of model checking:

  * [LTL](https://en.wikipedia.org/wiki/Linear_temporal_logic)
    and
    [CTL](https://en.wikipedia.org/wiki/Computation_tree_logic)
    are "temporal logic" notations, of which CTL is the most expressive.
    These can be used to test random predicates that range over the time
    progression of a system.  Think `AG p` to state that property `p`
    must hold for all future states, in all runtime variations, from the
    point in time at which it is activated.

  * Standard tests (are currently assumed to) exist to exclude various
    problematic situations:
      - [Deadlock](https://en.wikipedia.org/wiki/Deadlock)
        is the situation where processes all wait for
        others to make the first move, and end up being stuck.
      - [Livelock](https://en.wikipedia.org/wiki/Deadlock#Livelock)
        is the situation where processes are running around in circles,
        trying to do things and continually failing; they are burning
        computer time without actually achieving some progress.  Most of us
        know people who function like that too...
      - [Starvation](https://en.wikipedia.org/wiki/Starvation_(computer_science))
        describes the continued denial of resource access to processes,
        presumably as a result of locks; it is related to livelock.
      - [Boundedness](https://en.wikipedia.org/wiki/Petri_net#Boundedness)
        is used to determine how many tokens could at most be present in any
        place in the Petri net.  This can be used to prove that a limited-size
        integer can carry all the tokens that it might encounter.  It is
        possible to design for boundedness, which may be of interest to
        embedded designs.
      - [Well-formedness](https://en.wikipedia.org/wiki/Well-formed_Petri_net)
        helps to describe a Petri net in terms of symbolic notation, which
        bypasses the state explosion and replaces it with a mathematical
        derivation technique.  TODO:Study.
      - [Reachability](https://en.wikipedia.org/wiki/Petri_net#Reachability)
        is a very general property, namely being able to reach a certain
        computational state from a given initial state (or "marking" as it
        is called in Petri nets).  The concept relies heavily on the state
        space concept, and appears to inherit its scalability issues.  If
        you are looking for generic validation tools, then a symbolic approach
        such as LTL or CTL is more likely to produce results quickly.

We have not tried any tools yet.  If you tried tools that we should add,
please let us know.  If you developed tools to surpass what is reported, we
would like to hear about it.

There are
[model checking contests](http://mcc.lip6.fr/results.php)
to keep developers of model checking software at the peak of their abilities.
Lovely, an academic variation of economic principles at work, without the
downsides of the market model `:-)`

A lot is said about LoLa, but it appears to have dropped from the net.
Truly a great loss.

We are more than a little interested in
[PNMC](https://ahamez.github.io/pnmc/)
which is founded on
[libssd](https://github.com/ahamez/libsdd)
documented
[here](http://reasoning.cs.ucla.edu/sdd/doc/sdd-beginner-manual.pdf)
[and here](http://reasoning.cs.ucla.edu/sdd/doc/sdd-advanced-manual.pdf).
PNMC demonstrates
[impressive performance](https://www.researchgate.net/publication/220444223_Building_Efficient_Model_Checkers_using_Hierarchical_Set_Decision_Diagrams_and_Automatic_Saturation)
[read more](https://www.researchgate.net/publication/307585994_A_Symbolic_Model_Checker_for_Petri_Nets_pnmc)
as a result of its symbolic nature, thus evading the state space explosion.

