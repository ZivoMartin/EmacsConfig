#ifndef SAFE_RENDERER
#define SAFE_RENDERER

#include "../include.hpp"

class Env;

class Renderer {

public:

	Renderer(Env* env, Lablib* lablib);
	Env* get_env();
	virtual void render() = 0;
	virtual void handdle_keypress() = 0;
	virtual void init_lablib(Lablib* lablib) = 0;
	SDL_Window* get_win();
	SDL_Renderer* get_ren();
	Lablib* get_lablib();
	bool is_active(SDL_Keycode c);
	
private:
	Env* env;
	
};




#endif
