
#include "common/winutil.h"

/* see winutil for descriptions. */

void wu::make_tray_icon(int num, HWND hwnd, 
                    int cback, HICON iconsmall,
                    string status) {


  NOTIFYICONDATA nid;
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hWnd = hwnd;
  nid.uID = num;
  nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
  nid.uCallbackMessage = cback;
  nid.hIcon = iconsmall;
  strncpy(nid.szTip, -status, 60);

  Shell_NotifyIcon(NIM_ADD, &nid);

}

void wu::remove_tray_icon(int num, HWND hwnd) {

  NOTIFYICONDATA nid;
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hWnd = hwnd;
  nid.uID = num;
  nid.uFlags = 0;

  Shell_NotifyIcon(NIM_DELETE, &nid);

}


void wu::change_tray_icon_msg(int num, HWND hwnd, string status) {

  NOTIFYICONDATA nid;
  nid.cbSize = sizeof(NOTIFYICONDATA);
  nid.hWnd = hwnd;
  nid.uID = num;
  nid.uFlags = NIF_TIP;
  strncpy(nid.szTip, -status, 60);

  Shell_NotifyIcon(NIM_MODIFY, &nid);

}

void wu::alert(string msg) {
  MessageBox(0, -msg, "wu::alert", MB_OK);
}

/* this callback can be used for any dialog
   which just appears and doesn't do anything */

BOOL CALLBACK wu::info_dialog(HWND hwnd, UINT Message, 
                              WPARAM wParam, LPARAM lParam) {
  switch(Message) {
  case WM_INITDIALOG:
    return 1;
  case WM_COMMAND:
    switch(LOWORD(wParam)) {
    case IDOK:
      EndDialog(hwnd, IDOK);
      return 1;
    default:
      EndDialog(hwnd, IDCANCEL);
      return 1;
    }
    break;
  }
  return 0;
}
