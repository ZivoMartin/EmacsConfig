#include "MainBattle.hpp"
#include "../Env.hpp"
#include "../Sprite.hpp"
#include "../movables/Player.hpp"

#define SQUARE_COLOR 100, 200, 100, 100

MainBattle::MainBattle(Env* env, Lablib* lablib) : Renderer(env, lablib) {
	env->set_battle(this);
	SDL_Surface* surf = IMG_Load(BATTLE_SQUARE_PATH);
	square_text = SDL_CreateTextureFromSurface(env->get_ren(), surf);
	SDL_FreeSurface(surf);
	init_lablib(lablib);
	
	for(int i = 0; i < BATTLE_WIDTH; i++)
		for(int j = 0; j < BATTLE_HEIGHT; j++) 
			set(i, j, nullptr);
	set(BASE_PLAYER_X, BASE_PLAYER_Y, (Sprite*) env->get_player());
}

MainBattle::~MainBattle() {
	SDL_DestroyTexture(square_text);
}

void MainBattle::render() {
	int w = get_env()->win_width(), h = get_env()->win_height();
	tile_size = std::min(w, h) * BATTLE_TILE_SIZE;
	decal_w = w*0.5 - (tile_size*BATTLE_WIDTH)/2;
	decal_h = GRID_DECAL_TOP*h;
	for(int i = 0; i < BATTLE_WIDTH; i++)
		for(int j = 0; j < BATTLE_HEIGHT; j++) {
			Sprite* sprite = get(i, j);
			if (sprite != nullptr){
				sprite->Sprite::update();
				sprite->draw(decal_w+i*tile_size+tile_size/2-sprite->get_width()/2, decal_h+j*tile_size);
			}
		}
}

void MainBattle::handdle_keypress() {}

SDL_Texture* MainBattle::get_square_txt() {
	return square_text;
}

int MainBattle::get_dw() {
	return decal_w;
}

int MainBattle::get_dh() {
	return decal_h;
}

int MainBattle::get_ts() {
	return tile_size;
}

void MainBattle::set(int i, int j, Sprite* s) {
	board[i][j] = s;
}

Sprite* MainBattle::get(int i, int j) {
	return board[i][j];
}

void b_display_board(Button* grid) {
	MainBattle* battle = ((Env*) button_get_env(grid))->get_battle();
	int tile_size = battle->get_ts();
	button_set_height(grid, BATTLE_HEIGHT*tile_size);
	button_set_width(grid, BATTLE_WIDTH*tile_size);
	Env* env = (Env*) button_get_env(grid);
	button_set_rx(grid, ((float) battle->get_dw())/button_get_win_w(grid));
	env->get_battle()->display_board();
}

void MainBattle::display_board() {
	SDL_Surface* surf = TTF_RenderText_Blended(lablib_get_font(get_env()->get_lablib()), "", lablib_create_color(0, 0, 0, 255));
	SDL_Texture* text = SDL_CreateTextureFromSurface(get_ren(), surf);
	SDL_FreeSurface(surf);
	for(int i = 0; i < BATTLE_WIDTH; i++){
		for(int j = 0; j < BATTLE_HEIGHT; j++){
			SDL_Rect r = lablib_init_rect(i*tile_size + decal_w, j*tile_size + decal_h, tile_size);
			SDL_RenderCopy(get_ren(), get_square_txt(), NULL, &r);
		}
	} 
}

void MainBattle::click_on_grid() {
	Lablib* lablib = get_lablib();
	SDL_Point p = lablib_get_last_coordinate(lablib);
	int w = get_env()->win_width(), h = get_env()->win_height();
    p = {(p.x - decal_w)/tile_size, (p.y - decal_h)/tile_size};

}

#define NB_BUTTON 1
void MainBattle::init_lablib(Lablib* lablib) {
	Scene* battle = create_scene(lablib, NB_BUTTON);
	set_scene_background(lablib, battle, BATTLE_BG);
	Button* grid = scene_add_button(battle, 0.5, GRID_DECAL_TOP, 0, 0, "", &b_click_on_grid);
	button_set_display(grid, &b_display_board);
	get_env()->set_scene(battle, this);
}


void b_click_on_grid(Button* grid) {
	Env* env = (Env*) button_get_env(grid);
	env->get_battle()->click_on_grid();
}
