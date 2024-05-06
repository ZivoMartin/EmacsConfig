#include "TxtBubble.hpp"

#define BUBBLE_HEIGHT 0.2
#define BUBBLE_WIDTH 0.3

TxtBubble::TxtBubble(Env* env, const char* txt, SDL_Point p) : Static(env, txt, p, BUBBLE_WIDTH, BUBBLE_HEIGHT) {}

TxtBubble::~TxtBubble() {}

void TxtBubble::update() {
	Static::update();
	this->draw();
}
