
#ifndef __TOM7_FAKEHTTPD_H
#define __TOM7_FAKEHTTPD_H

#include <winsock2.h>

#include <windows.h>
#include <shlwapi.h>
#include <Commctrl.h>
#include "stdafx.h"

#include "./resource.h"
#include "common/stringhack.h"

#include "common/map.h"
#include "common/binmap.h"

#include "common/logwnd.h"

#include "common/winutil.h"

#define START_H 240
#define START_W 320

#define TRAY_ICON_FH 989
#define TRAY_ICON_CALLBACK WM_USER + 32

extern HINSTANCE g_hinst;

struct cbg {
  static void hidemainwindow();
  static void showmainwindow();

  static int exitprogram();

  static LRESULT CALLBACK window_callback(HWND hwnd, UINT message, 
					  WPARAM wParam, LPARAM lParam);

  static HICON icon_small;
  static HMENU contextmenu;
  static HWND mainhwnd;

};

#endif
