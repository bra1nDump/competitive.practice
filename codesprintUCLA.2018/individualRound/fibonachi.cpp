#include <iostream>
#include <vector>

using namespace std;

vector<int> fibonachi(int n) {
  vector<int> memorized;
  memorized.reserve(n + 2);

  memorized[1] = 1;
  memorized[2] = 1;

  for (int i = 3; i <= n; i += 1) {
    memorized[i] = (memorized[i - 1] + memorized[i - 2]) % 100000007;
  }

  return memorized;
}

int main() {
  int n;
  cin >> n;

  int buffer;
  vector<int> inputs;
  int maxInput = 0;
  for (int i = 0; i < n; i += 1) {
    cin >> buffer;
    inputs.push_back(buffer);

    if (maxInput < buffer) {
      maxInput = buffer;
    }
  }
  
  auto fib = fibonachi(maxInput);

  for (auto input : inputs) {
    cout << fib[input] << endl;
  }
  
}
