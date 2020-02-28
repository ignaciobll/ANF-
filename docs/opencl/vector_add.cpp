#include <algorithm>
#include <iostream>
#include <iterator>
#include <iomanip>

#ifdef __APPLE__
#include <OpenCL/cl.hpp>
#else
#include <CL/cl.hpp>
#endif

using namespace std;
using namespace cl;

Platform getPlatform();
Device getDevice(Platform platform, int i);
void print_vector(string name, int *vector, int n);

int main() {

  Platform default_platform = getPlatform();
  Device device = getDevice(default_platform, 0);

  Context context({device});
  Program::Sources sources;

  std::string kernel_code =
      "void kernel vector_add(global int* A, global int* B, global int* C, const unsigned int n) {"
      "  uint id = get_global_id(0);"
      "  if (id < n) {"
      "    C[id] = A[id] + B[id];"
      "  }"
      "}";

  sources.push_back({kernel_code.c_str(), kernel_code.length()});

  Program program(context, sources);

  if (program.build({device}) != CL_SUCCESS) {
    cout << "Error building: "
         << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(device) << std::endl;
    exit(1);
  }

  int max_wu = 25;
  int A[max_wu];
  int B[max_wu];
  int C[max_wu];

  for (int i = 0; i < max_wu; ++i) {
    A[i] = i;
    B[max_wu - i - 1] = i;
  }

  Buffer buffer_A(context, CL_MEM_READ_WRITE, sizeof(int) * max_wu);
  Buffer buffer_B(context, CL_MEM_READ_WRITE, sizeof(int) * max_wu);
  Buffer buffer_C(context, CL_MEM_READ_WRITE, sizeof(int) * max_wu);

  CommandQueue queue(context, device);
  queue.enqueueWriteBuffer(buffer_A, CL_TRUE, 0, sizeof(int) * max_wu, A);
  queue.enqueueWriteBuffer(buffer_B, CL_TRUE, 0, sizeof(int) * max_wu, B);
  queue.enqueueWriteBuffer(buffer_C, CL_TRUE, 0, sizeof(int) * max_wu, C);

  Kernel vector_add = Kernel(program, "vector_add");
  vector_add.setArg(0, buffer_A);
  vector_add.setArg(0, buffer_A);
  vector_add.setArg(1, buffer_B);
  vector_add.setArg(2, buffer_C);
  vector_add.setArg(3, max_wu);

  queue.enqueueNDRangeKernel(vector_add, NullRange, NDRange(max_wu), NullRange);

  queue.enqueueReadBuffer(buffer_C, CL_TRUE, 0, sizeof(int) * max_wu, C);

  print_vector("A", A, max_wu);
  print_vector("B", B, max_wu);
  print_vector("C", C, max_wu);
}

void print_vector(string name, int *vector, int n) {
  cout << name << " [";
  for (int i = 0; i < n; ++i) {
    cout << setw(2) << vector[i] << " ";
  }
  cout << "]" << endl;
}

Platform getPlatform() {
  std::vector<Platform> all_platforms;
  Platform::get(&all_platforms);

  if (all_platforms.size() == 0) {
    cout << "No platforms found. Check OpenCL installation!\n";
    exit(1);
  }

  return all_platforms[0];
}

Device getDevice(Platform platform, int i) {
  std::vector<Device> all_devices;
  platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);

  if (all_devices.size() == 0) {
    cout << "No devices found. Check OpenCL installation!\n";
    exit(1);
  }

  return all_devices[i];
}
