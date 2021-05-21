.PHONY: testAll testOne graph genAntlr loadTests run lines linesEachFile profile pgfplots

project_name=pipemimic
test_target=

arch=

litmus_test_dir=./litmus-tests-riscv/tests/non-mixed-size/BASIC_2_THREAD
litmus_tests=$(wildcard $(litmus_test_dir)/*.litmus)

java_srcs=$(shell cat ./javasrcs)
antlr_srcs=$(wildcard ./parser/*.g4)

graphs = $(shell find graphs -name '*.gv')
pngs = $(patsubst %.gv, %.png, $(graphs))

csv_files=./profiling/po-result.csv profiling/po-profiling.csv profiling/litmus-result.csv profiling/litmus-profiling.csv
log_file=./litmus-tests-riscv/model-results/herd.logs

testAll:
	mill -i --color false $(project_name).test

testOne:
ifdef test_target
	@echo "$(project_name).$(test_target)"
	mill -i --color false $(project_name).test.testOne $(project_name).$(test_target)
else
	@echo "<test target> argument missing"
endif

graph: $(pngs)

$(pngs): $(graphs)
	dot $(patsubst %.png,%.gv,$@) -Tpng -o $@

$(java_srcs): $(antlr_srcs)
	mill -i --color false parser.genAntlr

genAntlr: $(java_srcs)

loadTests: $(java_srcs) $(litmus_tests)
	mill -i --color false pipemimic.runMain pipemimic.LitmusFileTest $(litmus_tests)

run: $(java_srcs) $(litmus_tests)
ifdef arch
	mkdir -p graphs/$(arch)/sameAddr
	mkdir -p graphs/$(arch)/anyAddr
	mkdir -p graphs/$(arch)/litmus
	mill -i --color false pipemimic.runMain pipemimic.TestSuite $(arch) $(litmus_tests)
endif

$(csv_files): $(java_srcs) $(litmus_tests)
	mkdir -p profiling
	# generate data
	echo "#" $(shell date) > profiling/po-result.csv
	echo "#" $(shell date) > profiling/po-profiling.csv
	mill -i --color false pipemimic.runMain pipemimic.ProgramOrderTest
	echo "#" $(shell date) > profiling/litmus-result.csv
	echo "#" $(shell date) > profiling/litmus-profiling.csv
	mill -i --color false pipemimic.runMain pipemimic.LitmusSuite $(litmus_tests)

profile: $(csv_files)

pgfplots: $(csv_files) $(log_file)
	# generate tex file
	python scripts/po-profiling.py
	python scripts/litmus-profiling.py
	python scripts/po-result.py
	python scripts/litmus-result.py
	python scripts/litmus-compare.py

lines:
	( find ./pipemimic/ -name '*.scala' -print0 | xargs -0 cat ) | wc -l

linesEachFile:
	find ./pipemimic/ -name '*.scala' | xargs wc -l | sort -nr