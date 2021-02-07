
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#include <psapi.h>

#include "top.h"


using namespace std;

vector<string>
Top::Enumerate() {
  // TODO: Consider growing and retrying if the buffer is filled.
  static constexpr int MAX_PROCESSES = 16384;

  std::vector<DWORD> pids(MAX_PROCESSES, 0);
  DWORD bytes_needed = 0;
  
  
  if (!EnumProcesses(pids.data(), MAX_PROCESSES * sizeof (DWORD),
                     &bytes_needed)) {
    // (Shouldn't fail, even if there are too many to fit...)
    return {};
  }

  int num_processes = bytes_needed / sizeof (DWORD);

  vector<string> out;
  out.reserve(num_processes);
  for (int i = 0; i < num_processes; i++) {
    int pid = pids[i];

    HANDLE handle = OpenProcess(PROCESS_QUERY_INFORMATION |
                                PROCESS_VM_READ,
                                FALSE, pid);

    if (handle == nullptr)
      continue;

    // Process can have multiple "modules" but we just get
    // the first for simplicity.
    HMODULE module;
    DWORD bytes_needed;
    if (EnumProcessModules(handle, &module, sizeof (module),
                           &bytes_needed)) {
      char name[MAX_PATH + 1] = "";
      if (0 != GetModuleBaseNameA(handle, module, name, MAX_PATH)) {
        out.emplace_back(name);
      }
    }
    
    CloseHandle(handle);
  }

  return out;
}
