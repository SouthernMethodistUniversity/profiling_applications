cmake_minimum_required(VERSION 2.8)
project(mmm)
find_package(Armadillo REQUIRED)
include_directories(${ARMADILLO_INCLUDE_DIRS})
add_executable(mmm mmm.cpp)
set_target_properties(mmm PROPERTIES
    CXX_STANDARD 14
    CXX_STANDARD_REQUIRED ON
)
# Hint
# set(CMAKE_CXX_FLAGS "")
include_directories(${ARMADILLO_INCLUDE_DIRS})
target_link_libraries(mmm ${ARMADILLO_LIBRARIES})

