#include <stdlib.h>
#include "../Env.hpp"
#define ROCK_IMG_PATH "../res/rock_ground.png"

Map::Map(Env* env) {
	this->env = env;
	for (int i=0; i<MAP_WIDTH; i++)
		for(int j=0; j<MAP_HEIGHT; j++)
			map[i][j] = Rock;
	ground_textures[0] = (SDL_Texture*) cp(IMG_LoadTexture(env->get_ren(), ROCK_IMG_PATH));
}
Map::~Map() {
	for (auto &text: ground_textures) SDL_DestroyTexture(text);
}

void Map::draw() {
	int tile_size = std::max(env->win_width()/MAP_WIDTH, env->win_height()/MAP_HEIGHT);
	for (int i=0; i<MAP_WIDTH; i++){
		for(int j=0; j<MAP_HEIGHT; j++) {
			SDL_Rect r = {.x= i*tile_size, .y= j*tile_size, .w= tile_size, .h= tile_size};
			cc(SDL_RenderCopy(env->get_ren(), ground_textures[map[i][j]], NULL, &r));
		}
	}
 }

