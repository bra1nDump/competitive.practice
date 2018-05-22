#include <iostream>
#include <algorithm>
#include <vector>
#include <string>

using namespace std;

vector<int> input(int n) {
  vector<int> array;
  array.reserve(n);

  int buffer;
  for (int i = 0; i < n; i += 1) {
    cin >> buffer;
    if (buffer != 0) {
      array.push_back(buffer);
    }
    
  }

  return array;
}

int main() {
  int tests;
  cin >> tests;

  vector<long long> results;
  for (int i = 0; i < tests; i += 1) {
    int pings, teammates, pingTypes;
    cin >> pings >> teammates >> pingTypes;

    auto instabilities = input(teammates);
    auto pingStrengths = input(pingTypes);

    long long total = 0;
    for (int j = 0; j < pings; j += 1) {
      sort(instabilities.begin(), instabilities.end(), greater<int>());
      sort(pingStrengths.begin(), pingStrengths.end(), greater<int>());

      if (instabilities[0] == pingStrengths[0]) {
        
      } else if (instabilities[0] > pingStrengths[0]) {
        total += instabilities[0];
        pingStrengths[0] -= 1;
        instabilities[instabilities.size() - 1] -= 1;
      } else {
        total += pingStrengths[0];
        pingStrengths[pingStrengths.size() - 1] -= 1;
        instabilities[0] -= 1;
      }

      if (instabilities.back() == 0) {
        instabilities.pop_back();
      }
      if (pingStrengths.back() == 0) {
        pingStrengths.pop_back();
      }

      if (instabilities.empty() || pingStrengths.empty()) {
        break;
      }
    }

    results.push_back(total);
  }

  for (auto result : results) {
    cout << result << endl;
  }
}
