#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <utility>
#include <tuple>

using namespace std;

int unlock(int from, int to, vector<int>& signals) {
  int delta = to - from;

  int middle = from + (delta / 2);
  int signal = signals[middle];
  if (signal == 1) {
    return middle;
  } else if (signal == 2) {
    return unlock(from, middle, signals);
  } else {
    return unlock(middle + 1, to, signals);
  }
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

  vector<vector<int>> results;
  for (int i = 0; i < tests; i += 1) {
    auto signals1 = input(100);
    auto signals2 = input(100);
    auto signals3 = input(100);

    vector<int> combination;
    combination.push_back(unlock(0,99,signals1));
    combination.push_back(unlock(0,99,signals2));
    combination.push_back(unlock(0,99,signals3));

    results.push_back(combination);
  }

  for (auto result : results) {
    for (auto digit : result) {
      cout << digit << " ";
    }
    cout << endl;
  }
}
