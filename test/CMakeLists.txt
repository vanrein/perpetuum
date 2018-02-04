# Tests, most extensively against Erlang code.

set (PETRINETS_TESTED traffic_light_nightly kxover_client) #TODO# Might add: kxover_server.test
set (RUNTYPES_TESTED syncrun asyncrun applogic wildsum syncrun_delayed asyncrun_delayed backgrounded reflow_sentinel reflow_transmap)

set (PERPETUUM_LIBDIR ${CMAKE_BINARY_DIR}/erlang/perpetuum-${Perpetuum_VERSION})

add_custom_target (stub_applogic.beam ALL
	COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/stub_applogic.erl)

add_custom_command (OUTPUT ${PERPETUUM_LIBDIR}/test
	COMMAND cmake -E make_directory ${PERPETUUM_LIBDIR}/test)

add_custom_target (run_tests.erl ALL
	COMMAND cmake -E copy
		${CMAKE_CURRENT_SOURCE_DIR}/run_tests.erl
		${CMAKE_CURRENT_BINARY_DIR}
	COMMAND cmake -E copy
		${CMAKE_CURRENT_SOURCE_DIR}/run_tests.erl
		${PERPETUUM_LIBDIR}/test
	DEPENDS stub_applogic.beam
		${PERPETUUM_LIBDIR}/test)

foreach (PETRINET ${PETRINETS_TESTED})

	add_custom_target (${PETRINET}.beam ALL
		COMMAND ${CMAKE_SOURCE_DIR}/compiler/perpetuum
					${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.pnml
					${CMAKE_CURRENT_BINARY_DIR}
		COMMAND erlc -pa ${PERPETUUM_LIBDIR}/ebin ${PETRINET}.erl
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		DEPENDS perpetuum-codegen
			#BELOW# ${CMAKE_CURRENT_BINARY_DIR}/${PETRINET}.test
			#BELOW# ${PERPETUUM_LIBDIR}/test/${PETRINET}.test
			stub_applogic.beam)

	if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.test")

		add_custom_command (OUTPUT ${PERPETUUM_LIBDIR}/test/${PETRINET}.test ALL
			COMMAND cmake -E copy
				${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.test
				${PERPETUUM_LIBDIR}/test
			DEPENDS ${PERPETUUM_LIBDIR}/test)

		add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${PETRINET}.test ALL
			COMMAND cmake -E copy
				${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.test
				${CMAKE_CURRENT_BINARY_DIR})

		add_custom_target (${PETRINET}.test.installed
			DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${PETRINET}.test
				${PERPETUUM_LIBDIR}/test/${PETRINET}.test)

		add_dependencies (${PETRINET}.beam ${PETRINET}.test.installed)

	endif ()

	add_dependencies (run_tests.erl ${PETRINET}.beam)

	foreach (TESTRUN ${RUNTYPES_TESTED})

		add_test (NAME erlang-test-${PETRINET}-${TESTRUN}
			COMMAND run_tests.erl ${PETRINET} ${TESTRUN}
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

		set_property (TEST erlang-test-${PETRINET}-${TESTRUN}
			PROPERTY ENVIRONMENT "ERL_LIBS=${PERPETUUM_LIBDIR}")

	endforeach()

endforeach()

