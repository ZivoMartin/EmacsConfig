#include "Player.hpp"
#include "../Env.hpp"
#include "../Pnj.hpp"
#include "monsters/Monster.hpp"
#define IMG_PLAYER_PATH "../res/mageSheet.png"
#define PLAYER_WIDTH 0.07
#define PLAYER_HEIGHT 0.1
#define BASE_PLAYER_POS {17, 20}

Player::Player(Env* env) : Movable(env, IMG_PLAYER_PATH, BASE_PLAYER_POS, PLAYER_WIDTH, PLAYER_HEIGHT)  {
	this->set_current_sprite(0, 10);
}

Player::~Player() {}

void Player::action() {
	draw();
}

void Player::draw() {
	int x, y;
	SDL_GetWindowSize(get_env()->get_win(), &x, &y);
	Movable::draw(x/2, y/2);
}

void Player::collide(MapEntity* entity) {
	if (entity->get_can_interact_with_player()) 
		entity_around.push_back(entity);
	entity->collide(this);
}

void Player::collide(Monster* monster) {

}

void Player::collide(Pnj* pnj) {}

void Player::interact() {
	if (entity_around.size() > 0)
		entity_around[0]->interact_with_player();
	entity_around.clear();
}

