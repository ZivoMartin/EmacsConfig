#include "Static.hpp"
#include "../sprite_data.hpp"
#include "../movables/monsters/Monster.hpp"
#include "../movables/Player.hpp"
#include "../Env.hpp"


Static::Static(Env* env, const char* img_path, SDL_Point pos, float fx, float fy) : Sprite(env, img_path, pos, fx, fy) {}

Static::~Static() {}


void Static::draw() {
	SDL_Point p = get_env()->convert_coord_to_pixels(*get_pos());
	SDL_Point map_dim = get_env()->game_dim();
	if (p.x > -get_width() && p.y > -get_height() && p.x < map_dim.x && p.y < map_dim.y)
		draw(p.x, p.y);
}

void Static::draw(int x, int y) {
	SDL_Rect dest = (SDL_Rect){x, y, get_width()+10, get_height()};
  cc(SDL_RenderCopy(get_ren(), get_text(), NULL, &dest));
}


