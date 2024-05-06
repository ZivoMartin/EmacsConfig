#include <stdlib.h>

using namespace std;

#include "../movables/Player.hpp"
#include "Map.hpp"
#include "../Env.hpp"

#define ROCK_IMG_PATH "../res/rock_ground.png"
#define LIGHT_GRASS_IMG_PATH "../res/light_grass.png"
#define DARK_GRASS_IMG_PATH "../res/dark_grass.png"
#define LIGHT_GRASS_S_IMG_PATH "../res/light_grassSouth.png"
#define LIGHT_GRASS_N_IMG_PATH "../res/light_grassNorth.png"
#define MAP_FILE_PATH "../res/map.txt"

Map::Map(Env* env) {
	this->env = env;
	load_game();
	save_texture(ROCK_IMG_PATH, Rock);
	save_texture(LIGHT_GRASS_IMG_PATH, LightGrass);
	save_texture(LIGHT_GRASS_S_IMG_PATH, LightGrassSouth);
	save_texture(LIGHT_GRASS_N_IMG_PATH, LightGrassNorth);
}

Map::~Map() {
	for (auto &text: ground_textures) SDL_DestroyTexture(text);
}

void Map::save_texture(const char* path, int index) {
	ground_textures[index] =  (SDL_Texture*) cp(IMG_LoadTexture(env->get_ren(), path));
}

void Map::draw_square(int x, int y, int i, int j) {
	SDL_Rect r = {.x=x, .y=y, .w=PIXEL_TILE_SIZE, .h=PIXEL_TILE_SIZE};
	cc(SDL_RenderCopy(env->get_ren(), ground_textures[map[i][j]], NULL, &r));
}

void Map::draw() {
	int p = PIXEL_TILE_SIZE;
	SDL_Point* pos = env->get_player()->get_pos();
	int player_x = pos->x/p, 
		player_y = pos->y/p;
	int mx = pos->x%p - p/2,
		my = pos->y%p - p/2;
	int x=0, y=0;
	int width = env->win_width(), height = env->win_height();
	for (int il=width/2-mx - p, ir = width/2-mx - p; (il+p) > 0 || (ir-p) < width; il-=p, ir+=p){
		for (int jl=height/2-my - p, jr=height/2-my - p; (jl+p) > 0 || (jr-p) < height; jl-=p, jr+=p) {
			draw_square(il, jl, player_x-x, player_y-y);
			draw_square(ir, jr, player_x+x, player_y+y);
			draw_square(il, jr, player_x-x, player_y+y);
			draw_square(ir, jl, player_x+x, player_y-y);
			y += 1;
		}
		y = 0;
		x += 1;
	}
 }

void Map::load_game() {
	int i = 0, j = 0;
	ifstream f(MAP_FILE_PATH);
    char c;
    while(f.good()) {
		f.get(c);
		if (c == 10) {
			i += 1;
			j = 0;
		} else {
			if (c == 'L') 	   map[j][i] = LightGrass;
			else if (c == 'R') map[j][i] = Rock;
			else if (c == 'S') map[j][i] = LightGrassSouth;
			else if (c == 'N') map[j][i] = LightGrassNorth;
			j += 1;
		} 
    }
    f.close();
}
