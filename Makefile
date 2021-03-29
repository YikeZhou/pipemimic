.PHONY: testAll testOne initSubmodule graph clean

project_name = pipemimic
test_target =

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
	dot $(patsubst %.png, %.gv, $@) -Tpng -o $@