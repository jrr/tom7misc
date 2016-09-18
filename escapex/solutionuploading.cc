
#include "solutionuploading.h"
#include "message.h"
#include "prompt.h"
#include "client.h"
#include "http.h"
#include "../cc-lib/md5.h"
#include "../cc-lib/base64.h"
#include "menu.h"
#include "textbox.h"
#include "optimize.h"
#include "play.h"

void SolutionUploading::PromptUpload(Drawable *below,
                                     Player *plr, const string &lmd5,
                                     const Solution &sol,
                                     const string &msg,
                                     const string &name_,
                                     bool speedrec) {
  string s;
  Client::quick_txdraw td;

  Label message;
  message.text = msg;

  int IND = 2;

  TextInput name;
  name.indent = IND;
  name.question = "Name:";
  name.input = name_;
  name.disabled = speedrec;
  name.explanation =
    "Name this solution, briefly.";

  Toggle speed;
  speed.indent = IND;
  speed.disabled = speedrec;
  speed.checked = true;
  speed.question = "Speed Record Only";
  speed.explanation =
    "Upload this solution as a speed record entry only.\n"
    "It will be deleted if someone beats it.";

  TextBox desc(42, 7);
  desc.indent = IND;
  desc.question = "Description. " GREY "(optional)" POP;
  desc.explanation =
    speedrec?
    "If you want, you can describe your solution here.\n"
    "It's fine to upload a speed record without comment.":
    "Describe your solution here. This is inserted as a comment\n"
    "with the level. This is optional, but if the solution isn't\n"
    "interesting somehow, why are you uploading it?\n";

  VSpace spacer((int)(fon->height * 1.5f));
  VSpace spacer2((int)(fon->height * 1.5f));


  Okay ok;
  ok.text = "Upload Solution";

  Cancel can;

  PtrList<MenuItem> *l = nullptr;

  PtrList<MenuItem>::push(l, &can);
  PtrList<MenuItem>::push(l, &ok);
  PtrList<MenuItem>::push(l, &spacer);

  PtrList<MenuItem>::push(l, &speed);
  PtrList<MenuItem>::push(l, &desc);
  PtrList<MenuItem>::push(l, &name);
  PtrList<MenuItem>::push(l, &spacer2);
  PtrList<MenuItem>::push(l, &message);

  /* display menu */
  std::unique_ptr<Menu> mm = Menu::Create(below, "Upload solution?", l, false);
  InputResultKind res = mm->menuize();
  PtrList<MenuItem>::diminish(l);
  mm.reset();

  if (res == InputResultKind::OK) {

    if (speed.checked ||
        Message::Quick(0, "Are you sure you want to upload\n"
                       "   this solution without marking\n"
                       "   it as a " BLUE "speedrun" POP "?\n"
                       "\n"
                       "   You should only do this if the\n"
                       "   solution is interesting somehow.\n"
                       "\n",

                       "Upload anyway",
                       "Cancel")) {

      std::unique_ptr<HTTP> hh{Client::connect(plr, td.tx.get(), &td)};

      if (hh.get() == nullptr) {
        Message::No(&td, "Couldn't connect!");
        return;
      }

      string solcont = sol.ToString();

      formalist *fl = nullptr;

      /* XXX seems necessary! but in aphasia cgi? */
      formalist::pusharg(fl, "dummy", "dummy");
      formalist::pusharg(fl, "id", itos(plr->webid));
      formalist::pusharg(fl, "seql", itos(plr->webseql));
      formalist::pusharg(fl, "seqh", itos(plr->webseqh));
      formalist::pusharg(fl, "md", MD5::Ascii(lmd5));
      formalist::pusharg(fl, "name", name.input);
      formalist::pusharg(fl, "speedonly", speed.checked ? "1" : "0");
      formalist::pusharg(fl, "desc", desc.get_text());
      formalist::pushfile(fl, "sol", "sol.esx", solcont);

      td.say("Uploading..");
      td.draw();

      string out;
      if (Client::rpcput(hh.get(), UPLOADSOL_RPC, fl, out)) {
        if (speedrec)
          Message::Quick(&td, GREEN "Success! the record is yours!" POP,
                         "OK", "", PICS THUMBICON POP);
        else
          Message::Quick(&td, GREEN "Success!" POP,
                         "OK", "", PICS THUMBICON POP);
      } else {
        Message::No(&td, RED "Upload failed: " +
                    out + POP);
      }

      formalist::diminish(fl);
    }
  }
}
