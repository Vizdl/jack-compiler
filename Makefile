include Makefile-extras

# all the executables
EXECS= parser pretty codegen optimiser-e optimiser-r
TESTS=$(patsubst %,test-%,${EXECS})
MYALL:=${wildcard shared-*.cpp}
MYPARSER:=parser.cpp ${wildcard parser-*.cpp}
MYCODEGEN:=codegen.cpp ${wildcard codegen-*.cpp}
MYPRETTY:=pretty.cpp ${wildcard pretty-*.cpp}
MYOPTIMISER_E:=optimiser-e.cpp ${wildcard optimiser-e-*.cpp}
MYOPTIMISER_R:=optimiser-r.cpp ${wildcard optimiser-r-*.cpp}

# C++ 11 just in case
CXXFLAGS=--std=c++11 -I. -Iincludes

all: test 

notest: ${EXECS}

test: ${TESTS}

help:
	-@echo make options are one of:
	-@echo " all, parser, codegen, pretty, optimiser-e or optimiser-r"
	-@echo testing options are one of:
	-@echo " test-all, test-parser, test-codegen, test-pretty, test-optimiser-e or test-optimiser-r"
	-@echo

.PHONY: parser codegen pretty optimiser-e optimiser-r

# to test student versions of the programs
test-new:
	-@(bash bin/run-tests new loud)

test-parser: parser
	-@(bash bin/run-tests parser loud)

test-codegen: codegen
	-@(bash bin/run-tests codegen loud)

test-pretty: pretty
	-@(bash bin/run-tests pretty loud)

test-optimiser-e: optimiser-e
	-@(bash bin/run-tests optimiser-e loud)

test-optimiser-r: optimiser-r
	-@(bash bin/run-tests optimiser-r loud)

# to run tests with working versions of the programs
test-working: test-working-parser test-working-codegen test-working-pretty test-working-optimiser-e test-working-optimiser-r

test-working-parser:
	-@(bash bin/run-tests working-parser loud)

test-working-codegen:
	-@(bash bin/run-tests working-codegen loud)

test-working-pretty:
	-@(bash bin/run-tests working-pretty loud)

test-working-optimiser-e:
	-@(bash bin/run-tests working-optimiser-e loud)

test-working-optimiser-r:
	-@(bash bin/run-tests working-optimiser-r loud)

clean:
	-rm -rf lib/*/parser lib/*/codegen lib/*/pretty lib/*/optimiser-e lib/*/optimiser-r

parser: lib/$(CS_ARCH)/parser

lib/$(CS_ARCH)/parser: ${MYPARSER} ${MYALL} lib/$(CS_ARCH)/lib.a
	${CXX} ${CXXFLAGS} -o $@ $^

codegen: lib/$(CS_ARCH)/codegen

lib/$(CS_ARCH)/codegen: ${MYCODEGEN} ${MYALL} lib/$(CS_ARCH)/lib.a
	${CXX} ${CXXFLAGS} -o $@ $^

pretty: lib/$(CS_ARCH)/pretty

lib/$(CS_ARCH)/pretty: ${MYPRETTY} ${MYALL} lib/$(CS_ARCH)/lib.a
	${CXX} ${CXXFLAGS} -o $@ $^

optimiser-e: lib/$(CS_ARCH)/optimiser-e

lib/$(CS_ARCH)/optimiser-e: ${MYOPTIMISER_E} ${MYALL} lib/$(CS_ARCH)/lib.a
	${CXX} ${CXXFLAGS} -o $@ $^

optimiser-r: lib/$(CS_ARCH)/optimiser-r

lib/$(CS_ARCH)/optimiser-r: ${MYOPTIMISER_R} ${MYALL} lib/$(CS_ARCH)/lib.a
	${CXX} ${CXXFLAGS} -o $@ $^

