
#ifndef __TOM7_WINUTIL_H
#define __TOM7_WINUTIL_H

#include <windows.h>
#include "common/stringhack.h"

struct wu {
 public:
  static void make_tray_icon(int num, HWND hwnd, 
                             int cback, HICON iconsmall,
                             string msg);

  static void remove_tray_icon(int num, HWND hwnd);
  
  static void change_tray_icon_msg(int num, HWND hwnd, string msg);

  static void alert(string msg);

  static BOOL CALLBACK info_dialog(HWND hwnd, UINT Message, 
				   WPARAM wParam, LPARAM lParam);


};


#endif
