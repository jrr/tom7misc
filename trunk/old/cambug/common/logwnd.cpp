
#include "common/logwnd.h"
/* #include "time.h" */

/* XXX fix this so I can include an icon */
// #include "./resource.h"

#define LOGWNDCLASS "logwnd"

#define EDITBOX_ID 1

#define LOGWND_W 640
#define LOGWND_H 320

#define EDITBOX_LIP_W 0
#define EDITBOX_LIP_H 16 + 16

#define WNDMAXLEN 4096
#define REMOVEAMT 32

/* dispatch to the proper instance. */

static BESTMAP<HWND, logwnd*, &icomp> logwnd_dispatcher;

long CALLBACK logwnd_callback(HWND hwnd, UINT message, 
			      WPARAM wparam, LPARAM lparam) {
  logwnd * that = 0;
  
  if (logwnd_dispatcher.find (hwnd, that)) {
    return that->callback(hwnd, message, wparam, lparam);
  } else {
    /* ut oh! */

    switch(message) {

    case 36:
    case 129:
    case 131:
    case 28:
    case 1:
      return DefWindowProc(hwnd, message, wparam, lparam);

      /* precreate messages. Since we haven't been assigned a
         window handle yet (ugh), just assume that we want to
         do the default thing. */
      break;
    default:
      MessageBox(0, -stringf("didn't find logwnd on msg %d!...", message),
		 "error!", 
		 MB_OK | MB_SYSTEMMODAL);
      exit(-1);
    }
  }
}

void showcell(HWND h, logwnd * l) {
  MessageBox(0, -stringf("hwnd: %p, logwnd: %p", h, l), "hi", 
	     MB_OK | MB_SYSTEMMODAL);
}

struct logwnd_class {

  logwnd_class(HICON lg, HICON sm) {
    WNDCLASSEX WndClass;

    WndClass.cbSize        = sizeof(WNDCLASSEX);
    WndClass.style         = NULL;
    WndClass.lpfnWndProc   = logwnd_callback;
    WndClass.cbClsExtra    = 0;
    WndClass.cbWndExtra    = 0;
    WndClass.hInstance     = g_hinst;
    WndClass.hIcon         = lg;
    WndClass.hCursor       = 0;
    WndClass.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
    WndClass.lpszMenuName  = 0;
    WndClass.lpszClassName = LOGWNDCLASS;
    WndClass.hIconSm       = sm;
    
    if(!RegisterClassEx(&WndClass)) {
      MessageBox(0, "logwnd: Window Registration Failed!", "Error!",
		 MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      exit(-1);
    } else {
      /*
      MessageBox(0, "logwnd: registered!", "hi!",
		 MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      */
    }
  }
};

/* register the class once at startup. */
logwnd_class * logwnd_factory = 0;


void logwnd::init (string title) {
  /*  HICON icon_wq = LoadIcon(g_hinst, MAKEINTRESOURCE(ICON_WQ));*/

  if (title == "") title = "log window";

  if (!logwnd_factory) logwnd_factory = new logwnd_class(largeico, smallico);

  hwnd = CreateWindowEx(WS_EX_CLIENTEDGE,
			LOGWNDCLASS,
			-title,
			WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT, 
			LOGWND_W, LOGWND_H,
			NULL, NULL, g_hinst, NULL);

  /* put it in the map for the callback dispatcher */
  logwnd_dispatcher.insert(hwnd, this);  

  if(hwnd == NULL) {
    MessageBox(0, "logwnd: Window Creation Failed!", "Error!",
	       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    exit(-1);
  }

  editbox = CreateWindowEx(WS_EX_CLIENTEDGE,
			   "EDIT",
			   "editbox name",
			   ES_MULTILINE | ES_WANTRETURN | ES_AUTOVSCROLL | 
			   WS_VSCROLL | 
			   ES_READONLY | ES_NOHIDESEL | 
			   WS_CHILD | WS_VISIBLE | WS_BORDER,
			   0, 0,
			   LOGWND_W - EDITBOX_LIP_W, LOGWND_H - EDITBOX_LIP_H,
			   hwnd,
			   (HMENU)EDITBOX_ID,
			   g_hinst,
			   NULL);

  if(editbox == NULL) {
    MessageBox(0, "logwnd: editbox creation failed!", "Error!",
	       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    exit(-1);
  }

  SendMessage(editbox, WM_SETTEXT, 0, 
	      (LPARAM) 
	      -stringf("[ log window : %ld ]\r\n", editbox)); 

  ShowWindow(hwnd, SW_SHOWNORMAL);
  UpdateWindow(hwnd);
}

long logwnd::callback(HWND h, UINT message, 
		      WPARAM wparam, LPARAM lparam) {

  switch(message) {
  case WM_CLOSE:
    hide();
    return 1;
  case WM_SIZE: {
    int w = LOWORD(lparam);
    int h = HIWORD(lparam);
    MoveWindow(editbox, 0, 0, 
	       w - EDITBOX_LIP_W, h - EDITBOX_LIP_H,
	       1);
    }
  /* FALLTHROUGH */
  default:
    return DefWindowProc(h, message, wparam, lparam);
  }
}

void logwnd::removetop() {
  SendMessage(editbox, EM_SETSEL, (WPARAM) 0, (WPARAM) REMOVEAMT);
  SendMessage(editbox, EM_REPLACESEL, 0, (LPARAM) "");
}

void logwnd::addstring(const char * in) {
  int lastchar;

  lastchar = SendMessage(editbox, WM_GETTEXTLENGTH, 0, 0);
  
  while (lastchar > WNDMAXLEN) {
    removetop();
    lastchar = SendMessage(editbox, WM_GETTEXTLENGTH, 0, 0);
  }

  SendMessage(editbox, EM_SETSEL, (WPARAM) lastchar, (WPARAM) lastchar);
  SendMessage(editbox, EM_REPLACESEL, 0, (LPARAM) in);
}

void logwnd::logs(string in) {
  addstring(-(in + "\r\n"));
}

void logwnd::clear() {
  SendMessage(editbox, WM_SETTEXT, 0, (LPARAM)"");
}

char logwnd_logf_buf[8192];
void logwnd::logf(const char * fmt, ...) {
   va_list ap;
  va_start(ap, fmt);
  (void) _vsnprintf(logwnd_logf_buf, 8191, fmt, ap);
  va_end(ap);
  addstring(logwnd_logf_buf);
}

void logwnd::dump() {

  int * i=(int*)this;

  for(int x = 0; x < (sizeof (logwnd) >> 2); x ++) {
    MessageBox(0, -stringf("Bytes at %p: %02X %02X %02X %02X (= %ld)",
			   i, ((*i) >> 24) & 255, ((*i) >> 16) & 255,
			   ((*i) >> 8) & 255, (*i) & 255, *i), "hi", MB_OK);
    i++;
  }

}

void logwnd::show() {
  ShowWindow(hwnd, SW_SHOW);
  ShowWindow(hwnd, SW_RESTORE);
}

void logwnd::hide() {
  ShowWindow(hwnd, SW_HIDE);
}
