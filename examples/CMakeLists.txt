cmake_minimum_required(VERSION 3.16)
project(emacs_c_demo LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_executable(emacs_c_demo
  demo_c.c
)
