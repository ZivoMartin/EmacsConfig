#ifndef SAFE_MOVABLE
#define SAFE_MOVABLE
#include "../include.hpp"
#include "../sprite_data.hpp"
#include "../Sprite.hpp"

#define BASE_SPEED 3
#define BASE_FRAME_SPEED 10

class Env;

class Movable : public Sprite {

public:
	Movable(Env* env, const char* img_path, SDL_Point pos, float fx, float fy);
	~Movable();
	virtual void update() override;
	virtual void action() = 0;
	virtual void move(int dx, int dy);
	virtual void move(Direction dir);
	virtual void attack(int attack_number);
	virtual void draw(int x, int y) override;
	virtual void draw() override;
	int get_speed();
	void set_speed(int s);
		
	int get_sprite_col();
    int get_sprite_row();
	void set_current_sprite(int x, int y);
	void set_sprite_col(int x);
	void set_sprite_row(int y);
	void inc_sprite_col();

	void set_frame_speed(int new_frame_speed);
	void update_frame();
	bool occupated();
	void set_activity(bool act);
	
private:
	int speed;
	SDL_Point current_sprite = {0, 10};
	bool has_move = false;
	int frame_speed = BASE_FRAME_SPEED;
	bool is_doing_something = false;
	int attack_sprite_row = 3;
	int nb_sprite_col = 1;
	Direction current_dir = Bot;
	int attack_sprites_number[NB_SPELL][2] = {SPELL_0_SPRITE, SPELL_1_SPRITE, SPELL_2_SPRITE};
};

#endif
