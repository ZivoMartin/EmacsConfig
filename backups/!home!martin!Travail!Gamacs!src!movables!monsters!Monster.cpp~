#include "Golem.hpp"
#include "Player.hpp"
#include "../Env.hpp"
#define IMG_GOLEM_PATH "../res/golem.png"
#define GOLEM_WIDTH 0.15
#define GOLEM_HEIGHT 0.15

Golem::Golem(Env* env, SDL_Point pos) : Movable(env, IMG_GOLEM_PATH, pos) {}

Golem::~Golem() {}

void Golem::action() {
	this->draw();
}

void Golem::draw() {
	SDL_Point p = get_env()->convert_coord_to_pixels(*get_pos());
	SDL_Point map_dim = get_env()->game_dim();
	int gs = std::max(get_env()->win_width()*GOLEM_WIDTH, get_env()->win_height()*GOLEM_HEIGHT);
	if (p.x > -gs && p.y > -gs && p.x < map_dim.x && p.y < map_dim.y) {
		SDL_Rect r= {.x=p.x, .y=p.y, .w=gs, .h=gs};
		cc(SDL_RenderCopy(get_ren(), get_text(), NULL, &r));
	}
}
