cmake_minimum_required(VERSION 3.5)

project(Numerical-Methods-in-Fortran)
enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE RELEASE)
endif()

if(CMAKE_Fortran_COMPILER MATCHES "gfortran*")
    set(CMAKE_Fortran_FLAGS         "-Wall -Wextra -Wconversion -fdefault-real-16 -freal-8-real-16 -fdefault-integer-8")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -O0 -g3 -fbounds-check")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
endif()

set(TARGET main)
set(SOURCE_FILES src/main.f90)

add_library(lib SHARED src/Assert.f90 src/Commons.f90 src/Constants.f90
                       src/Distributions.f90 src/IO.f90 src/LinearAlgebra.f90
                       src/Plots.f90 src/Random.f90 src/RootFinding.f90
                       src/Testing.f90
        )

add_executable(${TARGET} ${SOURCE_FILES})
target_link_libraries(${TARGET} lib)
set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
