.PHONY: testAll testOne graph genAntlr loadTests run lines linesEachFile

project_name=pipemimic
test_target=

arch=

litmus_test_dir=./litmus-tests-riscv/tests/non-mixed-size/BASIC_2_THREAD
litmus_tests=$(wildcard $(litmus_test_dir)/*.litmus)

java_srcs=$(shell cat ./javasrcs)
antlr_srcs=$(wildcard ./parser/*.g4)

graphs = $(shell find graphs/$(arch) -name '*.gv')
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
ifdef arch
	dot $(patsubst %.png,%.gv,$@) -Tpng -o $@
endif

$(java_srcs): $(antlr_srcs)
	mill -i --color false parser.genAntlr

genAntlr: $(java_srcs)

loadTests: $(java_srcs) $(litmus_tests)
ifdef test_target
	mill -i --color false pipemimic.runMain pipemimic.execution.LitmusFileTest $(test_target)
else
	mill -i --color false pipemimic.runMain pipemimic.execution.LitmusFileTest $(litmus_tests)
endif

run: $(java_srcs) $(litmus_tests)
ifdef arch
	rm -r graphs/$(arch)/
	mkdir -p graphs/$(arch)/sameAddr
	mkdir -p graphs/$(arch)/anyAddr
	mkdir -p graphs/$(arch)/litmus
	mill -i --color false pipemimic.runMain pipemimic.TestSuite $(arch) $(litmus_tests)
endif

.PHONY: lt-dbg po-dbg

lt-dbg: $(java_srcs) $(litmus_tests)
ifdef arch
	rm -r graphs/$(arch)/litmus
	mkdir -p graphs/$(arch)/litmus
	mill -i --color false pipemimic.runMain pipemimic.LitmusTestDebug $(arch) $(litmus_tests)
endif

po-dbg: $(java_srcs)
ifdef arch
	rm -r graphs/$(arch)/sameAddr
	rm -r graphs/$(arch)/anyAddr
	mkdir -p graphs/$(arch)/sameAddr
	mkdir -p graphs/$(arch)/anyAddr
	mill -i --color false pipemimic.runMain pipemimic.PreservedProgramOrderDebug $(arch)
endif

.PHONY: ppo litmus pgfplots

ppo: $(java_srcs)
	mkdir -p profiling
	# generate data
	echo "#" $(shell date) > profiling/po-result.csv
	echo "#" $(shell date) > profiling/po-profiling.csv
	mill -i --color false pipemimic.runMain pipemimic.PreservedProgramOrderRelease

litmus: $(java_srcs) $(litmus_tests)
	mkdir -p profiling
	# generate data
	echo "#" $(shell date) > profiling/litmus-result.csv
	echo "#" $(shell date) > profiling/litmus-profiling.csv
	mill -i --color false pipemimic.runMain pipemimic.LitmusTestRelease $(litmus_tests)

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