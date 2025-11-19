# Makefile for CALC compiler

.PHONY: build clean test-interp test-compile run examples help

# Directories
TEST_DIR = tests
BUILD_DIR = build
SRC_DIR = src

# Build the project
build:
	dune build

# Clean everything
clean:
	dune clean
	rm -rf $(BUILD_DIR)
	rm -f *.o

# Setup test directory structure
setup:
	mkdir -p $(TEST_DIR)
	mkdir -p $(BUILD_DIR)

# Compile runtime libraries (put in build dir)
runtime:
	@mkdir -p $(BUILD_DIR)
	clang -c $(SRC_DIR)/mem_runtime.c -o $(BUILD_DIR)/mem_runtime.o
	clang -c $(SRC_DIR)/closure_runtime.c -o $(BUILD_DIR)/closure_runtime.o

# Run interpreter interactively
interp:
	dune exec calc

# Test interpreter with a file
test-interp:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make test-interp FILE=tests/mytest.calc"; \
	else \
		cat $(FILE) | dune exec calc; \
	fi

# Compile a file to LLVM (output in build dir)
compile: runtime
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make compile FILE=tests/mytest.calc"; \
	else \
		BASENAME=$$(basename $(FILE) .calc); \
		cat $(FILE) | dune exec calcc > $(BUILD_DIR)/$$BASENAME.ll; \
		echo "Generated $(BUILD_DIR)/$$BASENAME.ll"; \
	fi

# Compile and run a file
run: runtime
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make run FILE=tests/mytest.calc"; \
	else \
		BASENAME=$$(basename $(FILE) .calc); \
		cat $(FILE) | dune exec calcc > $(BUILD_DIR)/$$BASENAME.ll; \
		clang $(BUILD_DIR)/$$BASENAME.ll $(BUILD_DIR)/mem_runtime.o $(BUILD_DIR)/closure_runtime.o -o $(BUILD_DIR)/$$BASENAME 2>/dev/null || exit 1; \
		echo "Running $$BASENAME:"; \
		$(BUILD_DIR)/$$BASENAME; \
	fi

# Run all tests in tests/ directory (interpreter)
test-all-interp:
	@echo "Running all interpreter tests..."
	@for file in $(TEST_DIR)/*.calc; do \
		if [ -f "$$file" ]; then \
			echo "=== Testing $$file ==="; \
			cat "$$file" | dune exec calc; \
			echo ""; \
		fi \
	done

# Run all tests in tests/ directory (compiler)
test-all-compile: runtime
	@echo "Running all compiler tests..."
	@for file in $(TEST_DIR)/*.calc; do \
		if [ -f "$$file" ]; then \
			BASENAME=$$(basename $$file .calc); \
			echo "=== Testing $$BASENAME ==="; \
			cat "$$file" | dune exec calcc > $(BUILD_DIR)/$$BASENAME.ll 2>/dev/null; \
			clang $(BUILD_DIR)/$$BASENAME.ll $(BUILD_DIR)/mem_runtime.o $(BUILD_DIR)/closure_runtime.o -o $(BUILD_DIR)/$$BASENAME 2>/dev/null && $(BUILD_DIR)/$$BASENAME || echo "FAILED"; \
			echo ""; \
		fi \
	done

# Show LLVM output for a test
show-llvm:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make show-llvm FILE=tests/mytest.calc"; \
	else \
		cat $(FILE) | dune exec calcc; \
	fi

# Help
help:
	@echo "CALC Compiler - Organized Project Structure"
	@echo ""
	@echo "Directory Structure:"
	@echo "  tests/       - Test files (*.calc)"
	@echo "  build/       - Compiled outputs (.ll, executables, .o files)"
	@echo "  src/         - Source code"
	@echo ""
	@echo "Commands:"
	@echo "  make build                - Build the compiler"
	@echo "  make setup                - Create directory structure"
	@echo "  make examples             - Create example test files"
	@echo "  make runtime              - Compile C runtime libraries"
	@echo ""
	@echo "Testing:"
	@echo "  make test                 - Run all tests (compiler)"
	@echo "  make run FILE=tests/x.calc        - Compile and run one test"
	@echo "  make test-interp FILE=tests/x.calc - Run test with interpreter"
	@echo "  make compile FILE=tests/x.calc    - Compile to LLVM only"
	@echo "  make show-llvm FILE=tests/x.calc  - Show LLVM output"
	@echo ""
	@echo "Run all tests:"
	@echo "  make test-all-interp      - Test all with interpreter"
	@echo "  make test-all-compile     - Test all with compiler"
	@echo ""
	@echo "Interactive:"
	@echo "  make interp               - Run interpreter (prompt)"
	@echo ""
	@echo "Clean:"
	@echo "  make clean                - Remove build artifacts"
	@echo ""
	@echo "Examples:"
	@echo "  make examples             # Create example tests"
	@echo "  make run FILE=tests/test_double.calc"
	@echo "  make test                 # Run all tests"