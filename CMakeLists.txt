#
# CMakeLists.txt for Perpetuum
#

cmake_minimum_required (VERSION 2.8 FATAL_ERROR)
project ("Perpetuum")

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

if (${PETRINET_SINGLETOONS})
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

find_package (Git)

exec_program (
                ${GIT_EXECUTABLE}
                ARGS diff --quiet
                RETURN_VALUE GIT_HAVE_CHANGES
)

exec_program (
                ${GIT_EXECUTABLE}
                ARGS describe --tags --match 'version-*.*-*'
                OUTPUT_VARIABLE GIT_VERSION_INFO
)

string (
        REGEX REPLACE "^version-([1-9][0-9]*|0)[.]([1-9][0-9]*|0)-(.*)$"
        "\\1"
        GIT_VERSION_MAJOR
        ${GIT_VERSION_INFO}
)

string (
        REGEX REPLACE "^version-([1-9][0-9]*|0)[.]([1-9][0-9]*|0)-(.*)$"
        "\\2"
        GIT_VERSION_MINOR
        ${GIT_VERSION_INFO}
)

if (GIT_HAVE_CHANGES EQUAL 0)

        string (
                REGEX REPLACE "^version-([1-9][0-9]*|0)[.]([1-9][0-9]*|0)-(.*)$"
                "\\3"
                GIT_VERSION_PATCHLEVEL
                ${GIT_VERSION_INFO}
        )

        set (
                USER_SUPPLIED_PATCHLEVEL
                "${GIT_VERSION_PATCHLEVEL}"
                CACHE STRING "User-override for patch level under ${GIT_VERSION_MAJOR}.${GIT_VERSION_MINOR}"
        )

else()

        exec_program (
                date
                ARGS '+%Y%m%d-%H%M%S'
                OUTPUT_VARIABLE GIT_CHANGES_TIMESTAMP
        )
        set (GIT_VERSION_PATCHLEVEL "local-${GIT_CHANGES_TIMESTAMP}")
        message (WARNING "Git reports local changes, fixing patch level to local-${GIT_CHANGES_TIMESTAMP}")

        unset (USER_SUPPLIED_PATCHLEVEL CACHE)

endif()


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


#
# TESTS
#


enable_testing ()


#
# INSTALLATION
#



#
# PACKAGING
#

set (CPACK_PACKAGE_VERSION_MAJOR ${GIT_VERSION_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${GIT_VERSION_MINOR})
if (GIT_HAVE_CHANGES EQUAL 0)
        set (CPACK_PACKAGE_VERSION_PATCH ${USER_SUPPLIED_PATCHLEVEL})
else()
        set (CPACK_PACKAGE_VERSION_PATCH "local-${GIT_CHANGES_TIMESTAMP}")
endif()

set (CPACK_BUNDLE_NAME "perpetuum")
set (CPACK_PACKAGE_CONTACT "Rick van Rein <rick@openfortress.nl>")
set (CPACK_PACKAGE_VENDOR "ARPA2.net")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "Perpetuum generates asynchronous schedulers from Petri nets")
set (CPACK_PACKAGE_DESCRIPTION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/README.MD)
set (CPACK_GENERATOR DEB RPM)
set (CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})

include (CPack)
