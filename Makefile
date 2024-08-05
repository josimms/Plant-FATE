#-------------------------------------------------------------------------------
# executable name
TARGET := libpfate

# Julia setup
JULIA_PATH := /usr  # Update this to your Julia installation path

# files
SRCFILES  :=  $(filter-out src/RcppExports.cpp src/r_interface.cpp $(wildcard src/pybind*.cpp), $(wildcard src/*.cpp))
SRCFILES += src/julia_wrapper.cpp  # Add Julia wrapper
PYBINDFILES := $(wildcard src/pybind*.cpp)
HEADERS := $(wildcard src/*.tpp) $(wildcard include/*.h) $(wildcard tests/*.h)
# ------------------------------------------------------------------------------

# paths
EXTERNAL_DIR := external

# include and lib dirs (esp for cuda)
INC_PATH :=  -I./inst/include -I/usr/include/eigen3 -isystem ./inst/LBFGSpp/include -I./src 
INC_PATH += -I$(EXTERNAL_DIR)/phydro/inst/include \
            -I$(EXTERNAL_DIR)/libpspm/include \
            -I$(EXTERNAL_DIR)/flare/include \
            -I$(JULIA_PATH)/include/julia
LIB_PATH := -L$(EXTERNAL_DIR)/libpspm/lib -L./lib -L$(JULIA_PATH)/lib  # Add Julia lib path
PTH_PATH := $(shell python3 -m pybind11 --includes)

# flags
PROFILING_FLAGS = -g -pg
CPPFLAGS = -O3 -std=c++17 -Wall -Wextra -DPHYDRO_ANALYTICAL_ONLY -DUSE_JULIA $(PROFILING_FLAGS)
LDFLAGS =  $(PROFILING_FLAGS)

EIGEN_CFLAGS := $(shell pkg-config --cflags eigen3)
CPPFLAGS += $(EIGEN_CFLAGS)
CPPFLAGS += -I/usr/include/eigen3 -I/usr/local/include/eigen3 -I/opt/local/include/eigen3

EIGEN_PATH ?= /path/to/eigen
CPPFLAGS += -I$(EIGEN_PATH)

CPPFLAGS += -I/usr/include/uv
CPPFLAGS += -I$(JULIA_PATH)/src

CPPFLAGS +=    \
-pedantic-errors  -Wcast-align \
-Wcast-qual \
-Wdisabled-optimization \
-Wformat=2 \
-Wformat-nonliteral -Wformat-security  \
-Wformat-y2k \
-Wimport  -Winit-self   \
-Winvalid-pch   \
-Wmissing-field-initializers -Wmissing-format-attribute   \
-Wmissing-include-dirs -Wmissing-noreturn \
-Wpacked   -Wpointer-arith \
-Wredundant-decls \
-Wstack-protector \
-Wstrict-aliasing=2 \
-Wswitch-enum \
-Wunreachable-code \
-Wvariadic-macros \
-Wwrite-strings

CPPFLAGS += -Wno-sign-compare -Wno-unused-variable \
-Wno-unused-but-set-variable -Wno-float-conversion \
-Wno-unused-parameter

# libs
AR = ar
LIBS = -lpspm -ljulia  # Add Julia lib

# files
OBJECTS = $(patsubst src/%.cpp, build/%.o, $(SRCFILES))

all: dir external_libs init_julia $(TARGET) apps

external_libs:
	(cd $(EXTERNAL_DIR)/libpspm && $(MAKE))

.PHONY: init_julia
init_julia:
	@echo "Initializing Julia..."
	@R -e 'library(JuliaCall); julia_setup()'

python: dir $(TARGET)
	pip3 install .

dir:
	mkdir -p lib build tests/build bin

$(TARGET): $(OBJECTS)
	$(AR) rcs lib/$(TARGET).a $(OBJECTS) $(LIBS)	

$(OBJECTS): build/%.o : src/%.cpp $(HEADERS)
	g++ -c $(CPPFLAGS) $(INC_PATH) $< -o $@

libclean:
	rm -f $(TARGET) build/*.o lib/*.a src/*.o bin/* log.txt gmon.out

extclean:
	(cd $(EXTERNAL_DIR)/libpspm && $(MAKE) clean)

re: clean all

clean: libclean testclean julia_clean

julia_clean:
	@echo "Cleaning up Julia..."
	@R -e 'library(JuliaCall); julia_command("exit()")'

## EXECUTABLES (APPS) ##

APP_FILES = $(wildcard apps/*.cpp)
APP_OBJECTS = $(patsubst apps/%.cpp, build/%.o, $(APP_FILES))
APP_TARGETS = $(patsubst apps/%.cpp, bin/%, $(APP_FILES))

apps: dir $(TARGET) compile_apps
	@echo $(APP_TARGETS)

compile_apps: $(APP_TARGETS)

$(APP_TARGETS): bin/% : apps/%.cpp $(HEADERS)
	g++ $(LDFLAGS) $(CPPFLAGS) $(INC_PATH) $(LIB_PATH) -o $@ $(OBJECTS) $< $(LIBS) -lpfate 


## TESTING SUITE ##

TEST_FILES = tests/lho.cpp tests/pf_test.cpp tests/pf_test_evol.cpp #$(wildcard tests/*.cpp)
TEST_OBJECTS = $(patsubst tests/%.cpp, tests/%.o, $(TEST_FILES))
TEST_TARGETS = $(patsubst tests/%.cpp, tests/%.test, $(TEST_FILES))
TEST_RUNS = $(patsubst tests/%.cpp, tests/%.run, $(TEST_FILES))
ADD_OBJECTS =

check: dir external_libs $(TARGET) compile_tests clean_log run_tests

compile_tests: $(TEST_TARGETS)
	
clean_log:
	@rm -f log.txt

run_tests: $(TEST_RUNS)
	
$(TEST_RUNS): tests/%.run : tests/%.test
	@echo "~~~~~~~~~~~~~~~ $< ~~~~~~~~~~~~~~~~" >> log.txt
	@time ./$< && \
		printf "%b" "\033[0;32m[PASS]\033[m" ": $* \n"  || \
		printf "%b" "\033[1;31m[FAIL]\033[m" ": $* \n"

$(TEST_OBJECTS): tests/%.o : tests/%.cpp $(HEADERS)
	g++ -c $(CPPFLAGS) $(INC_PATH) $< -o $@

$(TEST_TARGETS): tests/%.test : tests/%.o $(HEADERS)
	g++ $(LDFLAGS) -o $@ $(LIB_PATH) $(OBJECTS) $(ADD_OBJECTS) $< $(LIBS) -lpfate 

testclean:
	rm -f tests/*.o tests/*.test

recheck: testclean check

.PHONY: $(TEST_RUNS) run_tests clean testclean
# ------------------------------------------------------------------------------


website:
	R -e "Sys.setenv(RSTUDIO_PANDOC='/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); print(Sys.getenv('RSTUDIO_PANDOC')); pkgdown::clean_site(); pkgdown::init_site(); pkgdown::build_home(); pkgdown::build_articles(); pkgdown::build_tutorials(); pkgdown::build_news()"

api:
	doxygen doxygen/Doxyfile

clean_api:
	rm -rf docs/html