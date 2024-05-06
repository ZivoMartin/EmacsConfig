#ifndef SAFE_MAIN_INCLUDE
#define SAFE_MAIN_INCLUDE

extern "C" {
	#include "../Lablib/include/settings.h"	
}
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>
#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <algorithm> 

#define MAP_WIDTH 50
#define MAP_HEIGHT 50
#define PIXEL_TILE_SIZE 100
#define NB_SPELL 3

enum Direction {
	Top,
	Left,
	Bot,
	Right
};

enum SceneIndex {
	Menu,
	Game,
	Battle,
	TotalScene
};


class Env;
class Pnj;
class MainGame;

typedef std::vector<std::pair<void (MainGame::*)(Pnj*), bool (MainGame::*)()>> ActionVec;

#endif
