
#ifndef __TOM7_LOGWND_H
#define __TOM7_LOGWND_H

#include "common/conlog.h"
#include <windows.h>
#include <shlwapi.h>
#include <Commctrl.h>

#include "common/binmap.h"

extern HINSTANCE g_hinst;

class logwnd : public conlog {

  HWND hwnd;
  HWND editbox;

  void addstring (const char *);
  void init(string);

  void logwnd::removetop();

  HICON largeico;
  HICON smallico;

 public:
  long callback(HWND h, UINT message, 
		WPARAM wparam, LPARAM lparam);

  virtual void logs(string in);
  virtual void logf(const char * fmt, ...);
  
  virtual void clear();

  logwnd() : largeico(0), smallico(0) { init("log window"); }
  logwnd(string title) : largeico(0), smallico(0) { init(title); }
  logwnd(string title, HICON lrg, HICON sml) :
    largeico(lrg), smallico(sml) { init(title); }
    

  virtual void show();
  virtual void hide();

  /* debug ... */
  void dump();
};

#endif
