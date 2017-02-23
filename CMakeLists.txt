#
# CMakeLists.txt for Perpetuum
#

cmake_minimum_required (VERSION 3.1 FATAL_ERROR)
project ("Perpetuum")

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)

include (MacroEnsureOutOfSourceBuild)
include (MacroAddUninstallTarget)
include (MacroGitVersionInfo)
include (MacroCreateConfigFiles)

set (CMAKE_MACOSX_RPATH 0)

option (DEBUG
	"Produce verbose about Petri net processing"
	OFF)

option (BUILD_SINGLE_THREADED
	"Build without atomic operations because Perpetuum runs in one thread"
	ON)

option (PETRINET_SINGLETONS
	"Optimise with singular instances for each Petri net defined"
	OFF)

if (${PETRINET_SINGLETONS})
	set (PETRINET_GLOBAL_NAME "" CACHE STRING
	     "Efficiently reference the Petri Net via a global variable name")
else ()
	unset (PETRINET_GLOBAL_NAME CACHE)
endif ()

option (PETRINET_FLAT_SCHEDULING
	"Schedule Petri net activity in a flat loop, avoid stack-based recursion"
	ON)

option (PETRINET_WITHOUT_NAMES
	"Save storage space by not storing name strings in the Petri Net"
	OFF)

if (${PETRINET_SINGLETONS})
	if (${PETRINET_GLOBAL_NAME STREQUAL "")
		unset (PETRINET_GLOBAL_NAME)
	endif ()
endif ()



#
# Dependencies
#

if (NOT ${BUILD_SINGLE_THREADED})
	find_package (Threads REQUIRED)
endif()

find_package (PkgConfig)

#TODO# find_package (cmph REQUIRED)
pkg_search_module (
        cmph REQUIRED
        cmph libcmph Cmph CMPH
)



#
# Version Information
#

get_version_from_git (Perpetuum 0.0.0)

#
# Building
#

if (${BUILD_SINGLE_THREADED})
	add_definitions (-DCONFIG_SINGLE_THREADED)
endif ()

if (${PETRINET_FLAT_SCHEDULING})
	add_definitions (-DPETRINET_FLAT_SCHEDULING)
else ()
	add_definitions (-DPETRINET_RECURSIVE_SCHEDULING)
endif ()

if (${PETRINET_WITHOUT_NAMES})
	add_definitions (-DPETRINET_WITHOUT_NAMES)
endif ()

if (${PETRINET_SINGLETONS})
	add_definitions (-DPETRINET_SINGLETONS)
	if (NOT ${PETRINET_GLOBAL_NAME} STREQUAL "")
		add_definitions (-DPETRINET_GLOBAL_NAME=${PETRINET_GLOBAL_NAME})
	endif ()
endif ()

include_directories (include)

#TODO# configure_file (
#TODO# 	contrib/pkgconfig/perpetuum.pc.in
#TODO# 	${PROJECT_BINARY_DIR}/perpetuum.pc
#TODO# 	@ONLY
#TODO# )

add_custom_target (perpetuum-codegen ALL
		COMMAND ${CMAKE_COMMAND} -E copy
			${CMAKE_SOURCE_DIR}/compiler/perpetuum-codegen.py
			${CMAKE_BINARY_DIR}/perpetuum-codegen)

create_config_files (Perpetuum)


#
# TESTS
#


enable_testing ()


#
# INSTALLATION
#

install (FILES ${CMAKE_SOURCE_DIR}/src/runtime.c
	       ${CMAKE_SOURCE_DIR}/src/flatsched.c
	       ${CMAKE_SOURCE_DIR}/src/evfire.c
	DESTINATION share/perpetuum)

install (FILES ${CMAKE_SOURCE_DIR}/include/perpetuum/model.h
	       ${CMAKE_SOURCE_DIR}/include/perpetuum/api.h
	DESTINATION include/perpetuum)

install (FILES ${CMAKE_BINARY_DIR}/perpetuum-codegen
	PERMISSIONS WORLD_EXECUTE
	DESTINATION bin)

add_uninstall_target ()


#
# PACKAGING
#

set (CPACK_BUNDLE_NAME "perpetuum")
set (CPACK_PACKAGE_CONTACT "Rick van Rein <rick@openfortress.nl>")
set (CPACK_PACKAGE_VENDOR "ARPA2.net")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "Perpetuum generates asynchronous schedulers from Petri nets")
set (CPACK_PACKAGE_DESCRIPTION_FILE ${CMAKE_SOURCE_DIR}/README.MD)
set (CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})

include (PackAllPossible)
include (CPack)
