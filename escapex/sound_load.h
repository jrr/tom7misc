/* Generated by packpng from sound.pack. DO NOT EDIT */
   sound_data[0] = Mix_LoadWAV("sounds" DIRSEP "redlight.wav");
   if (!sound_data[0]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "redlight.wav'\n");
      return;
   }
   sound_data[1] = Mix_LoadWAV("sounds" DIRSEP "greenlight.wav");
   if (!sound_data[1]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "greenlight.wav'\n");
      return;
   }
   sound_data[2] = Mix_LoadWAV("sounds" DIRSEP "bluelight.wav");
   if (!sound_data[2]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "bluelight.wav'\n");
      return;
   }
   sound_data[3] = Mix_LoadWAV("sounds" DIRSEP "poweroff.wav");
   if (!sound_data[3]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "poweroff.wav'\n");
      return;
   }
   sound_data[4] = Mix_LoadWAV("sounds" DIRSEP "error.wav");
   if (!sound_data[4]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "error.wav'\n");
      return;
   }
   sound_data[5] = Mix_LoadWAV("sounds" DIRSEP "whizz.wav");
   if (!sound_data[5]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "whizz.wav'\n");
      return;
   }
   sound_data[6] = Mix_LoadWAV("sounds" DIRSEP "click.wav");
   if (!sound_data[6]) {
      printf("Couldn't load sound: 'sounds" DIRSEP "click.wav'\n");
      return;
   }

