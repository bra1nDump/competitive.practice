#include <iostream>
#include <algorithm>
#include <vector>
#include <string>

using namespace std;

int maxNumberOfStudents(int jumps, vector<int> cs31, vector<int> cs32) {
  int max = 0;
  for (auto cs31Cource : cs31) {
    for (auto cs32Cource : cs32) {
      int buf = cs31Cource + cs32Cource;
      if (buf <= jumps && buf > max) {
        max = buf;
      }
    }
  }

  return max;
}

vector<int> input(int n) {
  vector<int> array;
  array.reserve(n);

  int buffer;
  for (int i = 0; i < n; i += 1) {
    cin >> buffer;
    array.push_back(buffer);
  }

  return array;
}

int main() {
  int tests;
  cin >> tests;

  vector<int> results;
  for (int i = 0; i < tests; i += 1) {
    int jumps;
    cin >> jumps;

    int cs31Count, cs32Count;
    cin >> cs31Count >> cs32Count;

    auto cs31 = input(cs31Count);
    auto cs32 = input(cs32Count);

    results.push_back(maxNumberOfStudents(jumps, cs31, cs32));
  }

  for (auto result : results) {
    cout << result << endl;
  }
}
