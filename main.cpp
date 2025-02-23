#include <iostream>
#include <thread>

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>

#include "Cpu.h"
#include "Cartridge.h"

/* We will use this renderer to draw into this window every frame. */

#include <vector>


int main(int argc, char **argv) {
    static SDL_Window *window;
    static SDL_Renderer *renderer;

    if (argc < 2) {
        std::cout<<"Usage: emu <rom_file>\n";
        return -1;
    }
    std::string fileName = argv[1];
    Cartridge cartridge(fileName);
    if (!cartridge.load()){
        std::cout<<"Failed to load cartridge\n";
        return -1;
    }

    BUS::Bus bus(cartridge);
    CPU::Cpu cpu(bus);
    cpu.executeNCycles(10);


    //std::thread cpuThread(&CPU::Cpu::run,cpu);

    /*SDL_Set AppMetadata("Example Renderer Clear", "1.0", "com.example.renderer-clear");

    if (!SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS )) {
        SDL_Log("Couldn't initialize SDL: %s", SDL_GetError());
        return SDL_APP_FAILURE;
    }

    window = SDL_CreateWindow("sdl3 hello world", 800, 600, 0 );
    if (window == nullptr ) {
        SDL_Log("SDL_CreateWindow failed: %s", SDL_GetError());
        return -2;
    }

    renderer = SDL_CreateRenderer(window, nullptr );
    if (renderer == nullptr ) {
        SDL_Log("SDL_CreateRenderer failed: %s", SDL_GetError());
        return -2;
    }

    SDL_Log( "Inicialized");

    SDL_Event event;
    int quit = 0;
    while ( !quit ) {
        while ( SDL_PollEvent( &event ) ) {
            switch ( event.type ) {
                case SDL_EVENT_QUIT:
                    SDL_Log( "SDL3 event quit" );
                    quit = 1;
                    break;
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 255,  255 );
        SDL_RenderClear(renderer);

        SDL_RenderPresent(renderer);
        SDL_Delay(1);
    }
    SDL_Log( "SDL3 shutdown");
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return SDL_APP_CONTINUE;  /* carry on with the program! */
}