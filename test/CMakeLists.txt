# Tests, most extensively against Erlang code.

# Note: Not yet using kxover_server, there is no .test for it
set (PETRINETS_TESTED traffic_light_nightly kxover_client)
set (RUNTYPES_TESTED syncrun asyncrun applogic wildsum syncrun_delayed asyncrun_delayed backgrounded)

add_custom_target (stub_applogic.beam ALL
	COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/stub_applogic.erl)

add_custom_target (run_tests.erl ALL
	COMMAND cmake -E copy ${CMAKE_CURRENT_SOURCE_DIR}/run_tests.erl ${CMAKE_CURRENT_BINARY_DIR}/run_tests.erl
	DEPENDS stub_applogic.beam)

foreach (PETRINET ${PETRINETS_TESTED})

	add_custom_target (${PETRINET}.test ALL
		cmake -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.test ${PETRINET}.test)

	add_custom_target (${PETRINET}.beam ALL
		COMMAND ${CMAKE_SOURCE_DIR}/compiler/perpetuum
					${CMAKE_CURRENT_SOURCE_DIR}/${PETRINET}.pnml
					${CMAKE_CURRENT_BINARY_DIR}
		COMMAND erlc -pa ../erlang/perpetuum/ebin ${PETRINET}.erl
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		DEPENDS perpetuum-codegen ${PETRINET}.test stub_applogic.beam)

	add_dependencies (run_tests.erl ${PETRINET}.beam)

	foreach (TESTRUN ${RUNTYPES_TESTED})

		add_test (NAME erlang-test-${PETRINET}-${TESTRUN}
			COMMAND run_tests.erl ${PETRINET} ${TESTRUN}
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	endforeach()

endforeach()

