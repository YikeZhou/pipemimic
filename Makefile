.PHONY: testAll testOne initSubmodule graph clean litmusPreview genAntlr loadTests runLitmusTests

project_name=pipemimic
test_target=

litmus_test_dir=./litmus-tests-riscv/tests/non-mixed-size/BASIC_2_THREAD
litmus_tests=$(wildcard $(litmus_test_dir)/*.litmus)

java_srcs=$(shell cat ./javasrcs)
antlr_srcs=$(wildcard ./parser/*.g4)

litmusPreview:
	cat $(litmus_tests) > preview.log

graphs = $(wildcard ./graphs/*.gv)
pngs = $(patsubst %.gv, %.png, $(graphs))

testAll:
	mill $(project_name).test

testOne:
ifdef test_target
	@echo "$(project_name).$(test_target)"
	mill $(project_name).test.testOne $(project_name).$(test_target)
else
	@echo "argument missing"
endif

initSubmodule:
	git submodule update --init

clean:
	rm graphs/*.gv graphs/*.png

graph: $(pngs)

$(pngs): $(graphs)
	dot $(patsubst %.png,%.gv,$@) -Tpng -o $@

$(java_srcs): $(antlr_srcs)
	mill parser.genAntlr

genAntlr: $(java_srcs)

loadTests: $(java_srcs) $(litmus_tests)
	mill pipemimic.runMain pipemimic.LitmusFileTest $(litmus_tests)

runLitmusTests: $(java_srcs) $(litmus_tests)
	mill pipemimic.runMain pipemimic.TestSuite $(litmus_tests)
