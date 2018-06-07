#include <iostream>
#include <vector>
#include <string>
#include <utility>
#include <algorithm>


using namespace std;

vector<int> selectSeats(string passengers) {
  vector<int> rowSelectionOrder;

  int nextEmpty = 0;
  vector<int> rowsWithIntroverts;
  for (auto passenger : passengers) {
    if (passenger == '1') {
      int selectedSeat = rowsWithIntroverts.back();
      rowsWithIntroverts.pop_back();
      rowSelectionOrder.push_back(selectedSeat);
    } else {
      rowsWithIntroverts.push_back(nextEmpty);
      rowSelectionOrder.push_back(nextEmpty);
      nextEmpty += 1;
    }
  }

  return rowSelectionOrder;
}

int main() {
  int n;
  string passengers;

  int weight;
  vector<pair<int,int>> weights;

  cin >> n;
  for (int i = 1; i <= n; i += 1) {
    cin >> weight;
    weights.push_back(pair<int,int>(i, weight));
  }

  cin >> passengers;

  sort(weights.begin(), weights.end(), [](pair<int,int> seat1, pair<int,int> seat2) {
      return seat1.second < seat2.second;
    });

  auto rows = selectSeats(passengers);
  
  for (auto row : rows) {
    cout << weights[row].first << " ";
  }
}
