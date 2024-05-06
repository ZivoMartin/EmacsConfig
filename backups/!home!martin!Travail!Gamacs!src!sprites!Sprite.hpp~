#ifndef SPRITE_SAFE
#define SPRITE_SAFE

#include "include.hpp"

#include "Updatable.hpp"

class Env;

class Sprite : virtual public Updatable {

public:
	Sprite(Env* env, const char* img_path, SDL_Point pos, float fx, float fy);
	Sprite();
	~Sprite();
	Env* get_env() const;
	SDL_Renderer* get_ren() const;
	SDL_Window* get_win() const;

	virtual void update() override;

	SDL_Point* get_pos() override;
	int get_width() override;
	int get_height() override;
	void set_width(int w);
	void set_height(int h);
	void set_size(int w, int h);
	SDL_Texture* get_text();
	
	float fx();
	float fy();
	
private:
	Env* env;
	int width, height;
	SDL_Texture* sprite_sheet;
	SDL_FPoint factors;
	SDL_Point pos;

};



#endif
