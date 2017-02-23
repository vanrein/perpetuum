# ADD_ASN1_HEADER(<headername> <groupname>)
#    Add a target to generate <headername>.h from the corresponding
#    ASN.1 file <headername>.asn1. The target is added as a dependency
#	 to <groupname>, which may be an empty target.
#
# Generation of C header files from ASN.1 sources
# via the asn2quickder tool. The macro add_asn1_headers()
# is the main API entry: give it a target name (e.g.
# a custom target added to the default build target
# through add_custom_target(group-of-asn-headers ALL) )
# and a list of names. Each name must name a .asn1 file
# (without the extension).
#
#    add_custom_target(my-headers ALL)
#    add_asn1_headers(my-headers rfc1 rfc2)
#
# This snippet requires files rfc1.asn1 and rfc2.asn1 to exist.

# Copyright 2017, Adriaan de Groot <groot@kde.org>
#
# Redistribution and use is allowed according to the terms of the two-clause BSD license.
#    https://opensource.org/licenses/BSD-2-Clause
#    SPDX short identifier: BSD-2-Clause

macro(add_asn1_header _headername _groupname)
# Generate the header file in <quick-der/headername.h>
# and install that header file to include/quick-der/headername.h.
	add_custom_command (OUTPUT quick-der/${_headername}.h
		COMMAND ${CMAKE_SOURCE_DIR}/tool/asn2quickder.py ${CMAKE_CURRENT_SOURCE_DIR}/${_headername}.asn1
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_headername}.asn1
		WORKING_DIRECTORY quick-der
		COMMENT "Build include file ${_headername}.h from ASN.1 spec")
	add_custom_target(${_headername}_asn1_h DEPENDS quick-der/${_headername}.h)
	install(FILES ${CMAKE_CURRENT_BINARY_DIR}/quick-der/${_headername}.h DESTINATION include/quick-der)
# Also add a test that builds against that header
	set(ASN1_HEADER_NAME quick-der/${_headername})
	configure_file(header-test.c.in ${CMAKE_CURRENT_BINARY_DIR}/${_headername}.c @ONLY)
	add_executable(${_headername}-test ${CMAKE_CURRENT_BINARY_DIR}/${_headername}.c)
	target_include_directories(${_headername}-test PUBLIC ${CMAKE_SOURCE_DIR}/include ${CMAKE_CURRENT_BINARY_DIR})
	add_dependencies(${_headername}-test ${_groupname})
	add_test(${_headername}-test ${_headername}-test)
endmacro()

macro(add_asn1_headers _groupname)
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/quick-der)
	foreach (_header ${ARGN})
		add_asn1_header(${_header} ${_groupname})
		add_dependencies(${_groupname} ${_header}_asn1_h)
	endforeach()
endmacro()

