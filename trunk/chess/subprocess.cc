#ifndef __MINGW32__
# error Only implemented for Windows.
#endif

#include "subprocess.h"

#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <strsafe.h>

#include <string>
#include <deque>
#include <memory>

#include "../cc-lib/base/logging.h"

using namespace std;

#define BUFSIZE 4096

namespace {
struct SubprocessImpl : Subprocess {

  bool Write(const string &data) override {
    DWORD written;
    if (!WriteFile(g_hChildStd_IN_Wr, data.data(), data.size(), &written, nullptr))
      return false;
    CHECK(written == data.size()) << "Expected writes to complete...";

    return true;
  }

  bool ReadLine(string *line) override {
    if (!lines.empty()) {
      *line = std::move(lines.front());
      lines.pop_front();
      return true;
    }

    for (;;) {
      DWORD num_read;
      if (!ReadFile(g_hChildStd_OUT_Rd, &cbuf, BUFSIZE, &num_read, nullptr))
        return false;

      for (int i = 0; i < num_read; i++) {
        if (cbuf[i] == '\n') {
          // Then partial_line contains a line.
          lines.push_back(std::move(partial_line));
          partial_line.clear();
        } else if (cbuf[i] == '\r') {
          // strip these in line-reading mode.
        } else {
          partial_line.push_back(cbuf[i]);
        }
      }

      // If we have anylines, then we can return.
      if (!lines.empty()) {
        *line = std::move(lines.front());
        lines.pop_front();
        return true;
      }
      // Otherwise, keep reading.
    }
  }

  virtual ~SubprocessImpl() {
    // CHECK?
    if (g_hChildStd_IN_Wr) {
      CloseHandle(g_hChildStd_IN_Wr);
      g_hChildStd_IN_Wr = nullptr;
    }

    if (g_hChildStd_OUT_Rd) {
      CloseHandle(g_hChildStd_OUT_Rd);
      g_hChildStd_OUT_Rd = nullptr;
    }

    // XXX What about the others?

    if (child_process_handle) {
      TerminateProcess(child_process_handle, 0);
      CloseHandle(child_process_handle);
      child_process_handle = nullptr;
    }
  }

  // A single read can produce more than one line; they get queued here.
  std::deque<string> lines;
  // And in the steady state, we also have a partial line.
  string partial_line;

  // temporary storage during reading.
  CHAR cbuf[BUFSIZE] = {};

  HANDLE g_hChildStd_IN_Rd = nullptr;
  HANDLE g_hChildStd_IN_Wr = nullptr;
  HANDLE g_hChildStd_OUT_Rd = nullptr;
  HANDLE g_hChildStd_OUT_Wr = nullptr;

  HANDLE child_process_handle = nullptr;
};
}  // namespace

Subprocess::~Subprocess() {}

Subprocess *Subprocess::Create(const string &filename) {
  SECURITY_ATTRIBUTES saAttr;
  // Set the bInheritHandle flag so pipe handles are inherited.
  saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = nullptr;

  // printf("Create Subprocess struct...\n");
  // fflush(stdout);

  std::unique_ptr<SubprocessImpl> sub{new SubprocessImpl};

  // XXX: Probably need to clean these up if e.g. CreateProcess fails?
  // Create a pipe for the child process's STDOUT.
  if (!CreatePipe(&sub->g_hChildStd_OUT_Rd, &sub->g_hChildStd_OUT_Wr, &saAttr, 0))
    return nullptr;

  // Ensure the read handle to the pipe for STDOUT is not inherited.
  if (!SetHandleInformation(sub->g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0))
    return nullptr;

  // Create a pipe for the child process's STDIN.
  if (!CreatePipe(&sub->g_hChildStd_IN_Rd, &sub->g_hChildStd_IN_Wr, &saAttr, 0))
    return nullptr;

  // Ensure the write handle to the pipe for STDIN is not inherited.
  if (!SetHandleInformation(sub->g_hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0))
    return nullptr;

  // printf("Created handles...\n");
  // fflush(stdout);

  // Now create the child process.
  PROCESS_INFORMATION piProcInfo;
  STARTUPINFO siStartInfo;

  // Set up members of the PROCESS_INFORMATION structure.
  ZeroMemory(&piProcInfo, sizeof (PROCESS_INFORMATION));

  // Set up members of the STARTUPINFO structure.
  // This structure specifies the STDIN and STDOUT handles for redirection.
  ZeroMemory(&siStartInfo, sizeof (STARTUPINFO));
  siStartInfo.cb = sizeof (STARTUPINFO);
  siStartInfo.hStdError = sub->g_hChildStd_OUT_Wr;
  siStartInfo.hStdOutput = sub->g_hChildStd_OUT_Wr;
  siStartInfo.hStdInput = sub->g_hChildStd_IN_Rd;
  siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

  // ugh, CreateProcess needs a non-const pointer
  string filename_copy = filename;

  // printf("CreateProcess...\n");
  // fflush(stdout);

  if (!CreateProcess(nullptr,
                     // command line
                     &filename_copy[0],
                     // process security attributes
                     nullptr,
                     // primary thread security attributes
                     nullptr,
                     // handles are inherited
                     TRUE,
                     // creation flags
                     0,
                     // use parent's environment
                     nullptr,
                     // use parent's current directory
                     nullptr,
                     // STARTUPINFO pointer
                     &siStartInfo,
                     // receives PROCESS_INFORMATION
                     &piProcInfo))
    return nullptr;

  // printf("CreateProcess...\n");
  // fflush(stdout);

  // TODO: Keep these in Subprocess struct.
  // Close handles to the child process and its primary thread.
  // Some applications might keep these handles to monitor the status
  // of the child process, for example.
  // CloseHandle(piProcInfo.hProcess);
  sub->child_process_handle = piProcInfo.hProcess;
  CloseHandle(piProcInfo.hThread);

  // printf("OK...\n");
  // fflush(stdout);

  return sub.release();
}
