cmake_minimum_required(VERSION 3.10)

project(OpenCL_Intro VERSION 0.1)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

find_package(OpenCL REQUIRED)

# Source code

## Info
set(opencl_info_src info.cpp)

add_executable(info ${opencl_info_src})
target_link_libraries(info ${OpenCL_LIBRARIES})

## Info
set(opencl_info_c_src info.c)

add_executable(infoc ${opencl_info_c_src})
target_link_libraries(infoc ${OpenCL_LIBRARIES})

## Vector Add
set(opencl_vector_add_src vector_add.cpp)

add_executable(vector_add ${opencl_vector_add_src})
target_link_libraries(vector_add ${OpenCL_LIBRARIES})
