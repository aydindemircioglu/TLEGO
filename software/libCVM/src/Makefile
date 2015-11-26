CXX ?= g++
CFLAGS = -fopenmp -Wall -Wconversion -O3 -fPIC
SHVER = 2
OS = $(shell uname)

all: cmake

cmake:  
	cd libCVM; cmake .; make all; 

	