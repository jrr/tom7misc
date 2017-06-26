#include <allegro.h>
int main (int argc, char**argv) {
allegro_init();
install_timer();
install_keyboard();
//detect_midi_driver(0);
install_sound(DIGI_AUTODETECT,MIDI_AUTODETECT,0);

     if (argc==2) {
          
     play_midi(load_midi(argv[1]),1);

     while (!keypressed()) ;

     } else { printf ("%s midifile.mid\n",argv[0]); }
exit(0);
}
