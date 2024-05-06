cmake_minimum_required(VERSION 3.7)
project(gamacs)


find_package(SDL2 REQUIRED)


set(SOURCES
    src/main.cpp
    src/Env.cpp
    src/movables/Player.cpp
    src/movables/Movable.cpp
    src/movables/monsters/Monster.cpp
    src/movables/monsters/Orc.cpp
    src/RegularAction.cpp
    src/view/Map.cpp
)

# Add an executable with the above sources
add_executable(${PROJECT_NAME} ${SOURCES})

target_link_libraries(gamacs ${SDL2_LIBRARIES} SDL2_image)

# Set the directories that should be included in the build command for this target
include_directories(${SDL2_INCLUDE_DIRS})
