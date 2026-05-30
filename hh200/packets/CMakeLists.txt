cmake_minimum_required(VERSION 4.3)
project(Packets C)

add_executable(Packets main.c mongoose.c)
target_link_libraries(Packets pthread dl)
