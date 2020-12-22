CXX = clang++
CXXFLAGS = -Wall -Wextra -O3 -std=c++17
CPPFLAGS = -I/usr/local/include -I${HOME}/local/include

.DEFAULT_GOAL := all
.PHONY: all clean

all: a.out
	@:

a.out: main.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

clean:
	$(RM) a.out