#include "Sprite.hpp"
#include "Env.hpp"

#define BASE_WIDTH 60
#define BASE_HEIGHT 60

Sprite::Sprite(Env* env, const char* img_path, SDL_Point pos, float fx, float fy) {
	this->env = env;
	this->pos = {pos.x*PIXEL_TILE_SIZE, pos.y*PIXEL_TILE_SIZE};
	SDL_Surface* surf = IMG_Load(img_path);
	if (!surf) 
		surf = TTF_RenderText_Blended(lablib_get_font(env->get_lablib()), img_path, (SDL_Color) {255, 255, 255, 255});  
	this->sprite_sheet = SDL_CreateTextureFromSurface(env->get_ren(), surf);
	SDL_FreeSurface(surf);
	set_width(BASE_WIDTH);
	set_height(BASE_HEIGHT);
	this->factors = {fx, fy};
}

Sprite::Sprite() {}

Sprite::~Sprite() {
	SDL_DestroyTexture(get_text());
}

void Sprite::update() {
	int win_size = std::min(get_env()->win_width(), get_env()->win_height());
	set_size(win_size*fx(), win_size*fy());
}

Env* Sprite::get_env() const {
	return env;
}

SDL_Renderer* Sprite::get_ren() const {
	return get_env()->get_ren();
}

SDL_Window* Sprite::get_win() const {
	return get_env()->get_win();
}


int Sprite::get_width() {
	return this->width;
}

int Sprite::get_height() {
	return this->height;
}

void Sprite::set_width(int w) {
	this->width = w;
}

void Sprite::set_height(int h) {
	this->height = h;
}

SDL_Point* Sprite::get_pos() {
	return &this->pos;
}

void Sprite::set_size(int w, int h) {
	set_width(w);
	set_height(h);
}

SDL_Texture* Sprite::get_text() {
	return sprite_sheet;
}

float Sprite::fx() {
	return factors.x;
}

float Sprite::fy() {
	return factors.y;
}
