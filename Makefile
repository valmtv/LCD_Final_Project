.PHONY: all build clean repl compile run

PROGRAMS_DIR = programs
BUILD_DIR = build

all: build

build:
	dune build
	@mkdir -p $(BUILD_DIR)

repl:
	dune exec calc

compile:
	@if [ -z "$(FILE)" ] || [ -z "$(OUTPUT)" ]; then \
		echo "Usage: make compile FILE=input.calc OUTPUT=output.ll"; \
		exit 1; \
	fi
	@if [ -f "$(FILE)" ]; then \
		INPUT_FILE="$(FILE)"; \
	elif [ -f "$(PROGRAMS_DIR)/$(FILE)" ]; then \
		INPUT_FILE="$(PROGRAMS_DIR)/$(FILE)"; \
	else \
		echo "Error: File not found: $(FILE)"; \
		exit 1; \
	fi; \
	BASENAME=$$(basename $$INPUT_FILE .calc); \
	mkdir -p $(BUILD_DIR)/$$BASENAME; \
	dune exec calcc < $$INPUT_FILE > $(BUILD_DIR)/$$BASENAME/$(OUTPUT)

run:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make run FILE=program.calc"; \
		exit 1; \
	fi
	@if [ -f "$(FILE)" ]; then \
		INPUT_FILE="$(FILE)"; \
	elif [ -f "$(PROGRAMS_DIR)/$(FILE)" ]; then \
		INPUT_FILE="$(PROGRAMS_DIR)/$(FILE)"; \
	else \
		echo "Error: File not found: $(FILE)"; \
		exit 1; \
	fi; \
	BASENAME=$$(basename $$INPUT_FILE .calc); \
	mkdir -p $(BUILD_DIR)/$$BASENAME; \
	dune exec calcc < $$INPUT_FILE > $(BUILD_DIR)/$$BASENAME/$$BASENAME.ll; \
	clang -c src/mem_runtime.c -o $(BUILD_DIR)/$$BASENAME/$$BASENAME.mem.o; \
	clang -c src/closure_runtime.c -o $(BUILD_DIR)/$$BASENAME/$$BASENAME.closure.o; \
	clang $(BUILD_DIR)/$$BASENAME/$$BASENAME.ll $(BUILD_DIR)/$$BASENAME/$$BASENAME.mem.o $(BUILD_DIR)/$$BASENAME/$$BASENAME.closure.o -o $(BUILD_DIR)/$$BASENAME/$$BASENAME; \
	$(BUILD_DIR)/$$BASENAME/$$BASENAME

clean:
	dune clean
	rm -rf $(BUILD_DIR)