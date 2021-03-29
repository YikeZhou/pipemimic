.PHONY: testAll testOne initSubmodule graph

project_name = pipemimic
test_target =

graphs = $(wildcard ./graphs/*.gv)
pngs = $(patsubst %.gv, %.png, $(graphs))

testAll:
	mill $(project_name).test

testOne:
ifdef test_target
	echo "$(project_name).$(test_target)"
	mill $(project_name).test.testOne $(project_name).$(test_target)
endif

initSubmodule:
	git submodule update --init

$(pngs): $(graphs)
	dot $^ -Tpng -o $@

graph: $(pngs)
