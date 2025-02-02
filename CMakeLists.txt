cmake_minimum_required(VERSION 3.28)
project(gbemu)

set(CMAKE_CXX_STANDARD 23)
#set(CMAKE_CXX_FLAGS "-Wall -O3 -march=native")

find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)

add_executable(gbemu main.cpp
        CPU.cpp
        CPU.h
        Cartridge.cpp
        Cartridge.h
        Bus.cpp
        Bus.h
)

target_link_libraries(gbemu PRIVATE SDL3::SDL3)
