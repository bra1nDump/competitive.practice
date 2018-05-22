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
    array.push_back(buffer);
  }

  return array;
}

vector<int> selectedDistricts(int contestants, vector<int> population) {
  vector<vector<pair<int,vector<int>>>> table(contestants + 1, vector<int>(population.size(), pair<int,vector<int>>(0, vector<int>())));
  for (int i = 1; i <= k; i += 1) {
    for (int j = 0; j < population.size(); j += 1) {
      if (j - 2 >= 0) {
        table[i-1][j-2]
      }
    }
  }
}

int main() {
  int tests;
  cin >> tests;

  vector<int> results;
  for (int i = 0; i < tests; i += 1) {
    int districts, contestants;
    cin >> districts >> contestants;

    auto population = input(districts);

    results.push_back(selectedDistricts(districts, contestants, population));
  }

  for (auto result : results) {
    cout << result << endl;
  }
}
