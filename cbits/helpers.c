#include "SDL.h"
#include "SDL_mixer.h"

// These were all macros in SDL_mixer.h.

extern DECLSPEC Mix_Chunk * SDLCALL
  Mix_LoadWAV_helper(
    char *file) {

  return Mix_LoadWAV_RW(SDL_RWFromFile(file, "rb"), 1);
}

extern DECLSPEC int SDLCALL
  Mix_PlayChannel_helper(
    int channel,
    Mix_Chunk *chunk,
    int loops) {

  return Mix_PlayChannelTimed(channel, chunk, loops, -1);
}

extern DECLSPEC int SDLCALL
  Mix_FadeInChannel_helper(
    int channel,
    Mix_Chunk *chunk,
    int loops,
    int ms) {

  return Mix_FadeInChannelTimed(channel, chunk, loops, ms, -1);
}
