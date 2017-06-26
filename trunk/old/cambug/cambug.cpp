
/* cambug is a small application to grab frames from a VFW device
   (and to potentially automate their upload to a web server somewhere.) */

#include "cambug.h"

#define PROGRAMNAME "cambug"

HINSTANCE g_hinst;

HWND cbg::mainhwnd = 0;
HICON cbg::icon_small = 0;
HMENU cbg::contextmenu = 0;

/* these are shared by some common files, so can't be
   encapsulated in the class */
conlog * log  = 0;

extern char cambug_id[];
char cambug_id[] = "$Id: cambug.cpp,v 1.1 2001/07/25 22:28:15 tom7 Exp $";

void cbg::hidemainwindow() {
    wu::change_tray_icon_msg(TRAY_ICON_FH, mainhwnd, PROGRAMNAME ": hidden");
    ShowWindow(mainhwnd, SW_HIDE);
}

void cbg::showmainwindow() {
    wu::change_tray_icon_msg(TRAY_ICON_FH, mainhwnd, PROGRAMNAME);
    ShowWindow(mainhwnd, SW_RESTORE);
    SetForegroundWindow(mainhwnd);
}

int cbg::exitprogram() {
  wu::remove_tray_icon(TRAY_ICON_FH, mainhwnd);
  return 1;
}

LRESULT CALLBACK cbg::window_callback(HWND hwnd, UINT message, 
				      WPARAM wParam, LPARAM lParam) {
  /* handle crashing explorer ;) */
  static UINT taskbar_restarted;

  switch(message) {
  case WM_CREATE:
    taskbar_restarted = RegisterWindowMessage(TEXT("TaskbarCreated"));
    break;
  case TRAY_ICON_CALLBACK:
      switch(lParam) {
      case WM_RBUTTONUP:
	/* make context menu */
	{
	  POINT mouse;
	  GetCursorPos(&mouse);
			
	  SetForegroundWindow(mainhwnd); /* hack, part 1 */
  
	  TrackPopupMenuEx(contextmenu, TPM_HORIZONTAL, 
			   mouse.x, mouse.y, mainhwnd, 0);
	  
	  PostMessage(mainhwnd,WM_NULL, 0, 0); /* hack, part 2 */
	}
	break;
      case WM_LBUTTONDBLCLK:
	showmainwindow();
	break;
      }

      return DefWindowProc(hwnd, message, wParam, lParam);
    break;
  case WM_COMMAND:
    switch(LOWORD(wParam)) {
    case MENU_EXIT:
      if (exitprogram()) {
	DestroyWindow(hwnd);
      }
      break;
    case MENU_SHOWLOG:
      log->show();
      break;
    case MENU_RESTORE:
      showmainwindow();
      break;
    }
    return 0;
  case WM_QUERYENDSESSION:
    if (exitprogram()) {
      DestroyWindow(hwnd);
    }
    break;
  case WM_LBUTTONDOWN:
    break;

  case WM_CLOSE:
    /* do NOT exit, just hide it. */

    hidemainwindow();
    break;
  case WM_DESTROY:
    wu::remove_tray_icon(TRAY_ICON_FH, mainhwnd);
    PostQuitMessage(0);
    break;
  default:
    if (message == taskbar_restarted) {
      /* re-add my icon */
      wu::make_tray_icon(TRAY_ICON_FH, mainhwnd, TRAY_ICON_CALLBACK, 
			 icon_small,
			 PROGRAMNAME);

    } else {
      return DefWindowProc(hwnd, message, wParam, lParam);
    }
  }
  return 0;
}


int APIENTRY WinMain(HINSTANCE hinstance,
                     HINSTANCE ignoreme,
                     char * args,
                     int ncmdshow) {

  WNDCLASSEX WndClass;
  HWND hwnd;
  MSG Msg;

  g_hinst = hinstance;

  HICON icon_fh = LoadIcon(g_hinst, MAKEINTRESOURCE(ICON_FH));
  cbg::icon_small =
	(HICON)
    LoadImage(g_hinst, MAKEINTRESOURCE(ICON_FH), IMAGE_ICON,
	      16, 16, LR_DEFAULTCOLOR);

  cbg::contextmenu = GetSubMenu
    (LoadMenu(g_hinst, MAKEINTRESOURCE(IDR_CONTEXTMENU)),
     0);

  WndClass.cbSize        = sizeof(WNDCLASSEX);
  WndClass.style         = NULL;
  WndClass.lpfnWndProc   = cbg::window_callback;
  WndClass.cbClsExtra    = 0;
  WndClass.cbWndExtra    = 0;
  WndClass.hInstance     = g_hinst;
  WndClass.hIcon         = icon_fh;
  WndClass.hCursor       = 0; 
  /* LoadCursor(g_hinst, MAKEINTRESOURCE(CURSOR_STUPID)); */
  WndClass.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
  WndClass.lpszMenuName  = MAKEINTRESOURCE(IDR_MENU1);
  WndClass.lpszClassName = PROGRAMNAME "_main";
  WndClass.hIconSm       = cbg::icon_small;


  if(!RegisterClassEx(&WndClass)) {
    MessageBox(0, "Window Registration Failed!", "Error!",
	       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return 0;
  }

  hwnd = CreateWindowEx(WS_EX_CLIENTEDGE,
			PROGRAMNAME "_main",
			PROGRAMNAME,
			WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT, START_W, START_H,
			NULL, NULL, g_hinst, NULL);

  if(hwnd == NULL) {
    MessageBox(0, "Window Creation Failed!", "Error!",
	       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return 0;
  }

  cbg::mainhwnd = hwnd;

  ShowWindow(hwnd, ncmdshow);
  UpdateWindow(hwnd);

  log = new logwnd(PROGRAMNAME " log", 
		   (HICON)
		   LoadImage(g_hinst, MAKEINTRESOURCE(ICON_LOG), IMAGE_ICON,
			     32, 32, LR_DEFAULTCOLOR),
		   (HICON)
		   LoadImage(g_hinst, MAKEINTRESOURCE(ICON_LOG), IMAGE_ICON,
			     16, 16, LR_DEFAULTCOLOR));

  wu::make_tray_icon(TRAY_ICON_FH, hwnd, TRAY_ICON_CALLBACK, 
		     cbg::icon_small,
		     PROGRAMNAME);

  while(GetMessage(&Msg, NULL, 0, 0)) {
    TranslateMessage(&Msg);
    DispatchMessage(&Msg);
  }
  return Msg.wParam;
}
