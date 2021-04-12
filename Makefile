.PHONY: testAll testOne graph clean genAntlr loadTests run lines linesEachFile

project_name=pipemimic
test_target=

arch=

litmus_test_dir=./litmus-tests-riscv/tests/non-mixed-size/BASIC_2_THREAD
litmus_tests=$(wildcard $(litmus_test_dir)/*.litmus)

java_srcs=$(shell cat ./javasrcs)
antlr_srcs=$(wildcard ./parser/*.g4)

graphs = $(shell find graphs -name '*.gv')
pngs = $(patsubst %.gv, %.png, $(graphs))

testAll:
	mill $(project_name).test

testOne:
ifdef test_target
	@echo "$(project_name).$(test_target)"
	mill $(project_name).test.testOne $(project_name).$(test_target)
else
	@echo "<test target> argument missing"
endif

clean:
	rm -r graphs

graph: $(pngs)

$(pngs): $(graphs)
	dot $(patsubst %.png,%.gv,$@) -Tpng -o $@

$(java_srcs): $(antlr_srcs)
	mill parser.genAntlr

genAntlr: $(java_srcs)

loadTests: $(java_srcs) $(litmus_tests)
	mill pipemimic.runMain pipemimic.LitmusFileTest $(litmus_tests)

run: $(java_srcs) $(litmus_tests)
ifdef arch
	mkdir -p graphs/$(arch)/sameAddr
	mkdir -p graphs/$(arch)/anyAddr
	mkdir -p graphs/$(arch)/litmus
	mill pipemimic.runMain pipemimic.TestSuite $(arch) $(litmus_tests)
endif

lines:
	( find ./pipemimic/ -name '*.scala' -print0 | xargs -0 cat ) | wc -l

linesEachFile:
	find ./pipemimic/ -name '*.scala' | xargs wc -l | sort -nr