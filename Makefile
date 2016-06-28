CXX ?= clang++
CXXFLAG = -g

test: src/reader_test.cpp src/reader.cpp src/reader.hpp
	$(CXX) -o bin/$@ $?
