
#include <string>

#include <windows.h>
#include <d2d1.h>
#include <d2d1helper.h>

#include <cmath>
#include <memory>
#include <string>
#include <vector>

#include "image.h"
#include "base/stringprintf.h"

// D2D1 example:
// https://docs.microsoft.com/en-us/windows/win32/direct2d/direct2d-quickstart


using namespace std;

#define assert(exp) \
  do { \
    if (! (exp) ) { Error("assertion failed: " #exp); exit(0); } \
  } while(0)

// ??
extern "C" IMAGE_DOS_HEADER __ImageBase;
static HINSTANCE GetInstance() {
  return (HINSTANCE)&__ImageBase;
}


void Error(const string &msg) {
  MessageBoxA(nullptr, msg.c_str(), "ERROR!", 0);
}

// Convert RGBA to BGRA and vice versa.
static void ColorSwap(vector<uint8> *xyza) {
  assert(xyza->size() % 4 == 0);
  for (int i = 0; i < (int)xyza->size(); i += 4) {
    // PERF: This approach does not appear to be vectorized with g++. :/
    const uint8 x = (*xyza)[i];
    const uint8 z = (*xyza)[i + 2];
    (*xyza)[i] = z;
    (*xyza)[i + 2] = x;
  }
}

struct App {
  // One window at a time.
  HWND window = nullptr;

  bool initialized = false;

  ID2D1Bitmap *test_bitmap = nullptr;
  
  // XXX?
  ID2D1Factory *d2d_factory = nullptr;
  ID2D1HwndRenderTarget *render_target = nullptr;

  std::unique_ptr<ImageRGBA> test_image;
  
  bool Init(const string &start_filename) {

    test_image.reset(ImageRGBA::Load(start_filename));
    if (test_image.get() == nullptr) {
      Error(StringPrintf("loading Image [%s]", start_filename.c_str()));
      return false;
    }

    const int initial_width = test_image->width;
    const int initial_height = test_image->height;
    
    // This is a "device independent" resource that can live the
    // length of the app.
    // In the example app this is all that CreateDeviceIndependentResources
    // does.
    //
    // XXX single threaded? no
    if (S_OK != D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
				  &d2d_factory)) {
      Error("d2d1");
      return false;
    }
      
    
    // Register the window class.
    // (XXX this is a weird initialization style. can we use .size = ?)
    WNDCLASSEX wcex = { sizeof(WNDCLASSEX) };
    wcex.style         = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc   = &WndProc;
    wcex.cbClsExtra    = 0;
    wcex.cbWndExtra    = sizeof(LONG_PTR);
    wcex.hInstance     = GetInstance();
    wcex.hbrBackground = nullptr;
    wcex.lpszMenuName  = nullptr;
    wcex.hCursor       = LoadCursor(nullptr, IDI_APPLICATION);
    wcex.lpszClassName = "FastView";

    RegisterClassExA(&wcex);

    // Because the CreateWindow function takes its size in pixels,
    // obtain the system DPI and use it to scale the window size.
    float dpiX, dpiY;

    // The factory returns the current system DPI. This is also the
    // value it will use to create its own windows.
    d2d_factory->GetDesktopDpi(&dpiX, &dpiY);

    // Create the window.
    window = CreateWindowA(
	"Fastview",
	"Fastview test window",
	WS_OVERLAPPEDWINDOW,
	CW_USEDEFAULT,
	CW_USEDEFAULT,
	// XXX Maybe we should not actually use the dpi. We would like the
	// window to be 1:1 with pixels in the image to start (or at least
	// an integer multiple!)
	static_cast<UINT>(ceil((float)initial_width * dpiX / 96.0f)),
	static_cast<UINT>(ceil((float)initial_height * dpiY / 96.0f)),
	nullptr,
	nullptr,
	GetInstance(),
	this);
    if (window == nullptr){
      Error("CreateWindow");
      return false;
    }
  
    ShowWindow(window, SW_SHOWNORMAL);
    UpdateWindow(window);

    UpdateRenderTarget();
        
    // XXX not just one hard-coded bitmap!
    D2D1_SIZE_U size = D2D1::SizeU(test_image->width, test_image->height);
    D2D1_BITMAP_PROPERTIES props;
    props.pixelFormat = 
      D2D1::PixelFormat(
	  // Note BGRA byte order. This is the best performance.
	  DXGI_FORMAT_B8G8R8A8_UNORM,
	  D2D1_ALPHA_MODE_IGNORE);
    props.dpiX = dpiX;
    props.dpiY = dpiY;
    
    if (S_OK != render_target->CreateBitmap(size, props, &test_bitmap)) {
      Error("CreateBitmap");
    }
    if (test_bitmap == nullptr) {
      Error("null bitmap?");
    }

    D2D1_RECT_U dst{.left = 0, .top = 0,
	.right = (UINT32)test_image->width,
	.bottom = (UINT32)test_image->width};

    ColorSwap(&test_image->rgba);
    if (S_OK !=
	test_bitmap->CopyFromMemory(&dst, test_image->rgba.data(),
				    test_image->width * 4)) {
      Error("bitmap CopyFromMemory");
    }

    // We may have gotten WM_PAINT before we finished initializing,
    // so force a redraw now that everything's loaded.
    Redraw();
    initialized = true;
    return true;
  }
  
  ~App() {
    if (d2d_factory) d2d_factory->Release();
    if (render_target) render_target->Release();    
  }

  void UpdateRenderTarget() {
    assert(window != nullptr);
    if (render_target) return;

    RECT rc;
    GetClientRect(window, &rc);

    D2D1_SIZE_U size = D2D1::SizeU(
	rc.right - rc.left,
	rc.bottom - rc.top);

    // See this lengthy doc for pixel formats:
    // https://docs.microsoft.com/en-us/windows/win32/Direct2D/supported-pixel-formats-and-alpha-modes
    D2D1_RENDER_TARGET_PROPERTIES props =
      D2D1::RenderTargetProperties();
    props.pixelFormat =
      D2D1::PixelFormat(
	  // Note BGRA byte order. This is the best performance.
	  DXGI_FORMAT_B8G8R8A8_UNORM,
	  D2D1_ALPHA_MODE_IGNORE);
    
    // Create a Direct2D render target.
    if (S_OK !=
	d2d_factory->CreateHwndRenderTarget(
	    props,
	    D2D1::HwndRenderTargetProperties(window, size),
	    &render_target)) {
      Error("CreateHwndRenderTarget");
    }
   
  }
  
  void MessageLoop() {
    MSG msg;
    while (GetMessage(&msg, nullptr, 0, 0)) {
      // XXX? I guess these are builtins...
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
  
  static LRESULT CALLBACK WndProc(
      HWND hWnd,
      UINT message,
      WPARAM wParam,
      LPARAM lParam) {

    // Error(StringPrintf("Wndproc message %d", message));

    if (message == WM_CREATE) {
      // Note that this is called before CreateWindow returns,
      // so the App hasn't even set its hwnd member yet.
      LPCREATESTRUCT pcs = (LPCREATESTRUCT)lParam;
      [[maybe_unused]] App *app = (App *)pcs->lpCreateParams;

      // Use SetWindowLongPtrW to attach the app to the window
      // (Could just do this after the CreateWindow call??)
      ::SetWindowLongPtrW(
	  hWnd,
	  GWLP_USERDATA,
	  reinterpret_cast<LONG_PTR>(app));

      return 1;
    } else {
      // Otherwise, get the app instance and dispatch
      // there.
      App *app = reinterpret_cast<App *>(
	  static_cast<LONG_PTR>(::GetWindowLongPtrW(
				    hWnd,
				    GWLP_USERDATA)));

      // If we haven't even gotten the CREATE message yet
      // (e.g. WM_NCCREATE) then just do the default.
      if (app == nullptr)
	return DefWindowProc(hWnd, message, wParam, lParam);

      // So too if Init hasn't finished. Note that it would be
      // possible to set up this member in WM_CREATE if we wanted.
      if (!app->initialized)
	return DefWindowProc(hWnd, message, wParam, lParam);
      
      if (hWnd != app->window) {
	Error(
	    StringPrintf(
		"I thought the hwnd passed to WndProc would "
		"always be the same as the one we saved as "
		"a member variable: %p vs %p",
		hWnd, app->window));
	exit(0);
      }

      return app->Proc(message, wParam, lParam);
    }
  }

  LRESULT Proc(
      UINT message,
      WPARAM wParam,
      LPARAM lParam) {
    // (Note: not called for WM_CREATE).
    assert(initialized);
    
    switch (message) {
    case WM_DESTROY:
      PostQuitMessage(0);
      return 1;
      
    case WM_PAINT: {
      Redraw();
      // this is what demo app does.. why?
      return 0;
    }
    case WM_DISPLAYCHANGE:
      InvalidateRect(window, nullptr, false);
      return 0;
      
      // XXX implement!
    case WM_SIZE:

    default:
      return DefWindowProc(window, message, wParam, lParam);
    }
  }

  void Redraw() {
    // (Note BeginPaint already validates it...?)
    render_target->BeginDraw();
    render_target->SetTransform(D2D1::Matrix3x2F::Identity());
    render_target->Clear(D2D1::ColorF(D2D1::ColorF::Black));
    D2D1_SIZE_F rtSize = render_target->GetSize();

    [[maybe_unused]] const int width = static_cast<int>(rtSize.width);
    [[maybe_unused]] const int height = static_cast<int>(rtSize.height);
      
    D2D1_RECT_F dst{.left = 0.0f, .top = 0.0f,
	.right = (float)test_image->width,
	.bottom = (float)test_image->height};

    render_target->DrawBitmap(
	test_bitmap,
	dst,
	1.0f,
	D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR,
	/* source rectangle is the same */
	dst);
      
    // XXX supposedly this can fail, and we need to recreate
    // resources in that case
    render_target->EndDraw();

    ValidateRect(window, nullptr);
  }
  
};

vector<string> ParseCommandLine(const char *cmdline) {
  // First phase: Process double quotes and spaces to tokenize
  // into the vector of arguments.
  vector<string> toks;
  // double quotes
  bool in_dq = false;
  string cur;
  for (int i = 0; cmdline[i]; i++) {
    const char c = cmdline[i];
    switch (c) {
    case '"':
      in_dq = !in_dq;
      cur += c;
      break;
    case ' ':
      if (in_dq) {
	cur += c;
      } else {
	toks.emplace_back(std::move(cur));
	cur.clear();
      }
      break;
    default:
      cur += c;
      break;
    }
  }
  toks.emplace_back(std::move(cur));
  cur.clear();
  
  // Now remove empties. This is the right thing to do because
  // consecutive spaces don't induce multiple arguments, and
  // empty arguments would be double-quoted.
  vector<string> ret;
  ret.reserve(toks.size());
  for (string &s : toks) {
    if (!s.empty()) ret.emplace_back(std::move(s));
  }
  toks.clear();

  // Finally, remove surrounding double-quotes if present.
  for (string &s : ret) {
    if (!s.empty() && s[0] == '"' && s[s.size() - 1] == '"') {
      s = s.substr(1, s.size() - 2);
    }
  }
  
  return ret;
}

int CALLBACK WinMain(HINSTANCE hInstance,
		     HINSTANCE hPrevInstance,
		     LPSTR lpCmdLine,
		     int nCmdShow) {
  // If we aren't even able to get a message box here, it's probably
  // because some necessary DLL isn't linked in. We get no feedback in
  // this case!
  // See dependent DLLs with: x86_64-w64-mingw32-objdump -x fastview.exe

  // MessageBoxA(nullptr, "hehe", "my programme", 0); 

  vector<string> args = ParseCommandLine(lpCmdLine);
  string filename = "test24.png";
  if (!args.empty()) filename = args[0];

  // Is in demo app, seems like a good idea for debugging at least?
  // Apparently this is enabled for all 64-bit processes anyway?
  (void)HeapSetInformation(nullptr, HeapEnableTerminationOnCorruption,
			   nullptr, 0);

  // I think just linking in the app stuff causes this to fail to run
  // because it's failing to find some DLL. can we get a better error
  // message from the system somehow?
  {
    App app;
    if (!app.Init(filename)) {
      MessageBoxA(nullptr, "init failed", "init failed", 0);
      return 1;
    }
    app.MessageLoop();
  }

  // MessageBoxA(nullptr, "done", "exit", 0);

  // TODO: We probably want to use Direct2D for this.
  // https://docs.microsoft.com/en-us/windows/win32/direct2d/direct2d-quickstart

  // Looks like we have float coordinates so maybe there's a little worry about
  // inexactness, but nearest neighbor should be fine? Note that all integers
  // within reasonable sizes for images/monitors are representable in float
  // (up to about 16 million), so don't stress about it.
  // (There is also D2D1_RECT_U which is 32-bit ints; can we use it? Seems no,
  // at least not with DrawBitmap.)
  
  // In Windows 8+ it looks like there are good interpolation modes:
  // https://docs.microsoft.com/en-us/windows/win32/api/d2d1_1/ne-d2d1_1-d2d1_interpolation_mode
  
  return 0;
}

/*
ID2D1RenderTarget::DrawBitmap(ID2D1Bitmap,constD2D1_RECT_F&,FLOAT,D2D1_BITMAP_INTERPOLATION_MODE,constD2D1_RECT_F&) method
ID2D1RenderTarget::DrawBitmap(ID2D1Bitmap,constD2D1_RECT_F&,FLOAT,D2D1_BITMAP_INTERPOLATION_MODE,constD2D1_RECT_F) method
ID2D1RenderTarget::DrawBitmap(ID2D1Bitmap,constD2D1_RECT_F,FLOAT,D2D1_BITMAP_INTERPOLATION_MODE,constD2D1_RECT_F) method
*/
