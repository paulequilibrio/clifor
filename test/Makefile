# Source extension
ext = f08
# Test driver basename
driver = driver
# Test binary executable
executable = driver.bin
# Source path
src = ../source
# Build path
build = ./build
# Fortran Compiler
fc = gfortran
# Fortran Compiler flags
flags = -J$(build) -std=f2008 -pedantic

# Get all clifor dependencies in a $(clifor) variable from file
include ../dependencies.make
# Object files relative to each source file in $(clifor)
clifor.obj = $(patsubst %, $(build)/%.o, $(clifor))

# All files matching test_*.$(ext) pattern in test directory
test.src = $(wildcard test_*.$(ext))
# Object files relative to each test file in $(test_src)
test.obj = $(patsubst test_%.$(ext), $(build)/test_%.o, $(test.src))

# If not exist, create build directory
$(shell mkdir -p $(build))


# Run tests
# 1>data/output.txt 2>data/error.txt
default: $(executable)
	./$(executable) -i ./data/input.json -o ./data/output.json

# Create executable binary
$(executable): $(clifor.obj) $(build)/fruit.o $(test.obj) $(build)/$(driver).o
	$(fc) $(flags) -o $(@) $(^)

# Compile clifor files
$(build)/%.o: $(src)/%.$(ext)
	$(fc) $(flags) -c $(<) -o $(@)

# Compile fruit test framework
$(build)/fruit.o: ../fruit/fruit.f90
	$(fc) $(flags) -c $(<) -o $(@)

# Compile test files
$(build)/test_%.o: test_%.$(ext)
	$(fc) $(flags) -c $(<) -o $(@)

# Compile test driver
$(build)/$(driver).o: $(driver).$(ext)
	$(fc) $(flags) -c $(<) -o $(@)

.PHONY: clean cleanall

clean:
	rm -rf $(build)
