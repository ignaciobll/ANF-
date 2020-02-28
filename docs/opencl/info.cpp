#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>

#ifdef __APPLE__
#include <OpenCL/cl.hpp>
#else
#include <CL/cl.hpp>
#endif

using namespace std;
using namespace cl;

Platform getPlatform();
Device getDevice(Platform platform, int i);

int main() {

  Platform default_platform = getPlatform();
  Device device = getDevice(default_platform, 0);

  string device_name = device.getInfo<CL_DEVICE_NAME>();
  string device_version = device.getInfo<CL_DEVICE_VERSION>();
  int max_compute_units = device.getInfo<CL_DEVICE_MAX_COMPUTE_UNITS>();
  int max_workgroup_size = device.getInfo<CL_DEVICE_MAX_WORK_GROUP_SIZE>();
  int max_work_units = max_compute_units * max_workgroup_size;

  cout << "Device name:          " << device_name << endl;
  cout << "Driver version:       " << device_version << endl;
  cout << "Compute Units:        " << max_compute_units << endl;
  cout << "Workgroup size (max): " << max_workgroup_size << endl;
  cout << "Work units (max):     " << max_compute_units << "x"
       << max_workgroup_size << " = " << max_work_units << endl;
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
