
#
# Erlang will be built using Erlang.mk, scripting for GNU make >= 4.0
#
# The result will then be harvested by CMake
#
# CMake invokes Erlang.mk as an ExternalProject
#
#TODO# Set the version number from CMake's deliberations with GIT.
#

cmake_minimum_required (VERSION 3.8)

include (ExternalProject)

set (PERPETUUM_LIBDIR ${CMAKE_BINARY_DIR}/erlang/perpetuum-${Perpetuum_VERSION})

add_custom_command (OUTPUT perpetuum-libdir
	COMMAND cmake -E make_directory ${PERPETUUM_LIBDIR}
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/src
		${PERPETUUM_LIBDIR}/src
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/include
		${PERPETUUM_LIBDIR}/include
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/ebin
		${PERPETUUM_LIBDIR}/ebin
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/doc
		${PERPETUUM_LIBDIR}/doc
	COMMAND cmake -E copy
		${CMAKE_CURRENT_SOURCE_DIR}/erlang.mk
		${PERPETUUM_LIBDIR}
	)

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/Makefile.in
		${PERPETUUM_LIBDIR}/Makefile
		@ONLY)

add_custom_target (erlang-perpetuum-bootstrap
	DEPENDS perpetuum-libdir)

ExternalProject_Add (erlang_build
	DEPENDS erlang-perpetuum-bootstrap
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}
	# erlang_build_prepare
	# SOURCE_DIR .
	# PREFIX erlang
	STAMP_DIR stamp
	TMP_DIR tmp
	# BUILD_IN_SOURCE 1
	BUILD_COMMAND ${GNUMAKE} -C ${PERPETUUM_LIBDIR} all
	INSTALL_COMMAND ${GNUMAKE} -C ${PERPETUUM_LIBDIR} app
	TEST_COMMAND ${GNUMAKE} -C ${PERPETUUM_LIBDIR} ct
	TEST_COMMAND COMMAND ${GNUMAKE} -C ${PERPETUUM_LIBDIR} plt dialyze)

