cmake_minimum_required(VERSION 3.7...3.27)

if(${CMAKE_VERSION} VERSION_LESS 3.12)
	cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
endif()

project(jitbox)

#message("Compiler being used: ${CMAKE_CXX_COMPILER_ID}")

#if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	#set(CMAKE_CXX_FLAGS "/EHsc")
#elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
	#set(CMAKE_CXX_FLAGS "-O3")
#elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	#set(CMAKE_CXX_FLAGS "-O3")
#else()
	#message(WARNING, "The Complax library has not been tested with this compiler before. Use at your own risk.")
#endif()

add_executable(jitbox src/jitbox.cpp)