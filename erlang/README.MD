# Building for Erlang

Based on [Erlang.mk](https://erlang.mk), this builds with GNU make 4.0 and up.

When building in here, you should manually copy `Makefile.in` to `Makefile`,
while setting the `@Perpetuum_VERSION` to the right value.  Then,

    gmake all

More usefully however, is to use the way this is wrapped into `CMakeLists.txt`
as an integral part of this project.  You are building up the right structures
in your binary directory's `erlang/perpetuum-${Perpetuum_VERSION}` subdirectory.

To use this, you should include the `erlang` directory in the path setup in the
`ERL_LIBS` environment variable before invoking Erlang.  You should then be
able to find the generic component with

   inluclude_lib( 'perpetuum/ebin/gen_perpetuum.erl' )

See the standard library call `code:lib_dir/1,2` for more information.
