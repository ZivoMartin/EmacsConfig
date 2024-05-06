#ifndef MAP_SAFE
#define MAP_SAVE

#include "../include.hpp"

class Env;

enum GroundType {
	Rock,
	LightGrass,
	DarkGrass,
	LightGrassNorth,
	LightGrassSouth,
	NbGround
};

class Map {
public:
	Map(Env* env);
	~Map();
	void draw();
	void draw_square(int x, int y, int i, int j);
		
private:
	void save_texture(const char* path, int index);
	
	Env* env;
	SDL_Texture* ground_textures[NbGround];
	GroundType map[MAP_WIDTH][MAP_HEIGHT];
	void load_game();
};


#endif
