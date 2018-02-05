# How to install Perpetuum

<img alt="Perpetuum Mobile" src="pix/240px-PerpetuumMobile.gif" style="float: right;"/>

> *Perpetuum should be easy to get started with.*

## Building Requirements

Perpetuum uses:

  * Git and a C compiler
  * Python 2.7 or higher
  * CMake version 3.2 or higher
  * Your favourite Make
  * GNU Make version 4.0 or higher (for Erlang.mk)
  * Erlang/OTP, *ideally* 20.1 or 21 or later
  * Our CMake helper library ARPA2CM

## Runtime Requirements

  * Erlang if you choose that platform
  * Your favourite event library if you choose C

## Building Steps

First install ARPA2CM:

 1. Clone [the repository](https://github.com/arpa2/ARPA2CM) and CD into it
 2. Create a build subdirectory, like `build`, and CD into it
 3. Prepare for building with CMake
 4. Run Make to Build
 5. Run Make to Install

In commandline details:

```
git clone https://github.com/arpa2/ARPA2CM
cd ARPA2CM
mkdir build
cd build
cmake ..
make
make install
```

To setup Perpetuum, act as follows:

 1. Clone [the repository](https://github.com/vanrein/perpetuum) and CD into it
 2. Create a build subdirectory, like `build`, and CD into it
 3. Prepare for building with CMake
 4. Run Make to Build
 5. Optionally run CTest

In commandline details:

```
git clone https://github.com/vanrein/perpetuum
cd perpetuum
mkdir build
cd build
cmake ..
make
ctest
```

## Harvesting the Build Outputs

Sorry... we don't have a `make install` phase at this time.

The thing is, since we're delivering mainly for Erlang, we're not quite sure what the best way is.  That's simply a matter of lack of experience with Erlang distribution and seeing too many options...

So we're currently ending the guided build process when we have built up an application directory.  We simply need to learn more, so you may well take notice that we're listening.  Feel free to post an
[Issue](https://github.com/vanrein/perpetuum/issues)
or
[Pull Request](https://github.com/vanrein/perpetuum/pulls)
if you have something to say on this!

## The Perpetuum Compiler

The compiler from PNML files to (a directory full of) files for C and Erlang (together, that surely needs work) you should pickup the contents of the `compiler` directory.  You will find the actual script there, named `perpetuum`, but also a few libraries that we depend on, but whose Python wrappers caused us more than a little headache.

## Compiler Library Dependencies

If you are a user, you may well benefit from keeping the included `cmph` and `pntools` libraries in your `$PYTHONPATH`.

If you are a packager, please make sure that `cmph` and `pntools` for Python are in good working order.  As we see it now, the tools suddenly start building code for a cache that appears to be time-bounded.  This may be specific to Mac OS X, but that is enough to want to work around the issues.

If you ar us, you will keep the two libraries sitting in the Git repository until the problems are solidly gone.

## Runtime Library for C

You will find the runtime library for C in `src` and the accompanying include files in `include`.  Please note that although we believe these libraries to be correctly implemented and well-aligned for use with your choice of event library, that the 1.0 release focusses primarily on Erlang.

## Runtime Library for Erlang

The build scripts setup an application structure under the build directory, as `erlang/perpetuum-VERSION`.  The normal way of using this is either cloning the `perpetuum-VERSION` directory into your distribution, or adding it to your `ERL_LIBS` environment variable.  Either way, you should see no errors about an undefined `gen_perpetuum` behaviour.

The application root holds an `ebin` with the generic behaviour `gen_perpetuum` that is included in the generated code, and that you will probably use when coding your application code around it.  You should be able to use `include_dir( "perpetuum/include/gen_perpetuum.hrl" )` to pull in the include file if you needed it.

## Known Bug in Erlang/OTP R19

Erlang/OTP has a bug in its runtime system `erts` in some or all
release 19 versions.  This bug is
[documented and solved](https://bugs.erlang.org/browse/ERL-450)
in versions 20.1, 21 and higher.

CTest will tell you if your release is having this issue by reporting
an error in a standard test case.

```
The following tests FAILED:
	 11 - erlang-test-traffic_light_nightly-reflow_transmap (Failed)
Errors while running CTest
```

If you care for the trouble of finding this nitty-gritty detail,
then by all means see
[issue#15](https://github.com/vanrein/perpetuum/issues/15)
for a lot of gory detail.

This is an indication that your Petri Nets cannot grow beyond what the compiler
prepared for you.  In practice, that is several steps up from the standard
provisioning, and you are unlikely to hit this roof.  Still, when you do, your
code may silently get the Transaction Sentinels wrong, effectively passing a
number of situations that your diagrams suppressed with inhibitor arcs.

Do have a look at the compiler output though; the arguments to `transmap/1`
and `sentinel/1` is the number of bits to hold the number of tokens in any
place of your Petri Net.  Chances are this will suffice.  The values given
here are computed with Python and not subject to the Erlang/OTP bug.

## When you see Opportunities

This is a young project, and an early release.  The 1.0 mark wants to make a statement on code stability, but it is not meant to claim that nothing could be done to improve the project.

So far, what we've done satisfies our own needs for other projects like
[KXOVER]()
and
[IdentityHub]()
but the range of application is boundless.  Although the supported range of the code is also boundless, there will still be scenario's that we haven't fulfilled.  Talk to us if you're in that kind of predicament; we'd really like you to take Petri Nets seriously as a way towards more structure for your next project, because we believe you will love it as much as we do.

Having said that, time is always a scarce resource, so feel free to post an
[Issue](https://github.com/vanrein/perpetuum/issues)
or
[Pull Request](https://github.com/vanrein/perpetuum/pulls)
if you have something to add.

Note that we have a few project already written out to some degree, you may find what you are looking for in there.  Talk to us if this is necessary for you, and certainly if you'd be willing to work on it interactively.

  * [Multiplicity for Inhibitors](https://github.com/vanrein/perpetuum/projects/2?)
    add to the expressiveness of Petri Nets.  We didn't need it yet, but you might.  These are our plans that didn't make it into the 1.0 release.
  * [Composing Petri Nets](https://github.com/vanrein/perpetuum/projects/3?)
    can be useful to manage complexity, and perhaps even allow for some degree of modularity.  Modules might listen to a transition that is filled elsewhere, and report back to another transition when done, for example.
  * Various [issues](https://github.com/vanrein/perpetuum/issues)
    have been partially worked out to describe persistency, hints between transitions and auto-firing them, control over the compiler, debugging information and improvement of scalability of introspection through the `marking()` and `canfire()` functions.

## When you find Bugs

*It might be a language/compiler bug, [as we found](https://github.com/vanrein/perpetuum/issues/15)...*

Seriously though, there is a thorough testing system, but it is not complete.  It can't be.  So your bugs are good opportunities to help improve the code.  When you need to report them, please
proceed as follows:

  * Be prepared to share your PNML files, if possible what made them for you
    such as a PNPRO file from GreatSPN.  We would like to include them in the
    source code to avoid delivering the same fault in the future.  If this is
    difficult for you, then please contact someone, possibly us, for a
    business arrangement.

  * Upgade to the latest `master` version in the
    [Git repo](https://github.com/vanrein/perpetuum).
    With the theoretic exception of a maintenance contract, we see no reason
    for filing an issue against an older version.

  * Reproduce your error with the `run_tests.erl` code.  This will make you
    construct a path that lands your system in problems.  Try the available
    test suites (see the [test](test/) directory for details) until you find
    one that reproduces your bug.

  * If you are inclined to self-help, then by all means try to simplify your
    diagram and/or trace to find the simplest problem demonstration you can find.
    This very, very often reveals the problem, and it is not always the toolkit's `;-)`
    We are still interested in adding persisting test case, even when you patched
    it yourself.

  * Post the result (Petri Net and .test trace) as an issue.  Please check
    first if a similar issue was already opened (and perhaps closed).  Mention
    the version you are running along with your post.
    You are always welcome to send in Pull Requests.

