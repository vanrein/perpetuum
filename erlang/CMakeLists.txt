
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

add_custom_command (OUTPUT perpetuum
	COMMAND cmake -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/perpetuum
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/src
		${CMAKE_CURRENT_BINARY_DIR}/perpetuum/src
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/include
		${CMAKE_CURRENT_BINARY_DIR}/perpetuum/include
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/ebin
		${CMAKE_CURRENT_BINARY_DIR}/perpetuum/ebin
	COMMAND cmake -E copy_directory
		${CMAKE_CURRENT_SOURCE_DIR}/docs
		${CMAKE_CURRENT_BINARY_DIR}/perpetuum/docs
	COMMAND cmake -E copy
		${CMAKE_CURRENT_SOURCE_DIR}/erlang.mk
		${CMAKE_CURRENT_SOURCE_DIR}/Makefile
			${CMAKE_CURRENT_BINARY_DIR}/perpetuum)

add_custom_target (erlang-perpetuum-bootstrap
	DEPENDS perpetuum)

ExternalProject_Add (erlang_build
	DEPENDS erlang-perpetuum-bootstrap
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}
	# erlang_build_prepare
	# SOURCE_DIR .
	# PREFIX erlang
	STAMP_DIR stamp
	TMP_DIR tmp
	# BUILD_IN_SOURCE 1
	BUILD_COMMAND ${GNUMAKE} -C ${CMAKE_CURRENT_BINARY_DIR}/perpetuum all
	INSTALL_COMMAND ${GNUMAKE} -C ${CMAKE_CURRENT_BINARY_DIR}/perpetuum app)

add_test (NAME erlang_common_tests
	COMMAND ${GNUMAKE} -C ${CMAKE_CURRENT_BINARY_DIR}/perpetuum ct)

add_test (NAME erlang_dialyze
	COMMAND ${GNUMAKE} -C ${CMAKE_CURRENT_BINARY_DIR}/perpetuum plt dialyze)

