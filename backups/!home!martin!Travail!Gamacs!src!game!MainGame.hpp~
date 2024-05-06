#ifndef SAFE_GAMESCENE
#define SAFE_GAMESCENE

#include "Renderer.hpp"
#include "../include.hpp"

class MapEntity;
class Map;
class TxtBubble;
class Player;
class Pnj;

class MainGame : public Renderer {

public:
	MainGame(Env* env, Lablib* lablib);
	~MainGame();
	void init_lablib(Lablib* lablib) override;
	void place_monsters();
	void init_pnj();
	void render() override;
	void handdle_keypress() override;
	void update_entities();	
	bool stop_inc_action() {return false;}
	Player* get_player();
	void talk(Pnj* pnj);
	void talk_and_inc(Pnj* pnj);
	void talk_and_reset(Pnj* pnj);
	
private:
	std::vector<MapEntity*> entities;
	Map* map;
	TxtBubble* txt_bubble = nullptr;
	
	
};


#endif

