#include "Monster.hpp"
#include "../Player.hpp"
#include "../../Env.hpp"

#define MONSTER_SPEED 1

Monster::Monster(Env* env, const char* sheet_p, SDL_Point pos, float w, float h) : Movable(env, sheet_p, pos, w, h) {
	set_speed(MONSTER_SPEED);
	set_interactible();
}

Monster::~Monster() {}

void Monster::action() {
	int now = get_env()->get_now();
	if (!occupated()) {
		if (now%200<50) 	  this->move(Bot);
		else if (now%200<100) this->move(Right);
		else if (now%200<150) this->move(Top);
		else if (now%200<200) this->move(Left);
	}
	printf("here\n");
	Movable::draw();
}


void Monster::collide(MapEntity* entity) {
	entity->collide(this);
}

void Monster::collide(Player* player) {

}

void Monster::interact_with_player() {
	get_env()->go_battle(this);
}
