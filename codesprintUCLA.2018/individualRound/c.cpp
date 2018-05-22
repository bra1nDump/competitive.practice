#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <map>

using namespace std;

int getComponentSize(vector<int> vertices, map<int,vector<int>>& edges, vector<bool>& discovered, int& discoveredCount) {
  vector<int> nextBreed;

  for (auto vertice : vertices) {
    auto childrenIt = edges.find(vertice);
    if (childrenIt != edges.end()) {
      vector<int> newChildren;
      for (auto child : childrenIt->second) {
        if (discovered[child] == false) {
          newChildren.push_back(child);
          discovered[child] = true;
          discoveredCount += 1;
        }
      }

      nextBreed.insert(nextBreed.end(), newChildren.begin(), newChildren.end());
    }
  }

  return vertices.size() + getComponentSize(nextBreed, edges, discovered, discoveredCount);
}

int factorial(int n) {
  if (n == 0) return 0;
  if (n == 1) return 1;
  return n * factorial(n - 1);
}

int connected(int rooms, int newEdges, map<int,vector<int>> edges) {
  vector<bool> discovered(rooms + 1, false);
  int discoveredCount = 0;

  vector<int> componentSizes;
  for (int i = 1; i < rooms; i += 1) {
    if (discoveredCount == rooms) {
      break;
    }

    if (discovered[i] == false) {
      discovered[i] = true;
      discoveredCount += 1;

      vector<int> singleton;
      singleton.push_back(i);
      componentSizes.push_back(getComponentSize(singleton, edges, discovered, discoveredCount));
    }
  }

  int edgesLeft = newEdges;
  int maxConnectedComponent = 0;
  sort(componentSizes.begin(), componentSizes.end(), greater<int>());

  vector<int> remainintComponentns;
  for (auto componentSize : componentSizes) {
    if (edgesLeft > 0) {
      maxConnected += componentSize;
      edgesLeft -= 1;
    } else {
      remainingComponents.push_back(componentSize);
    }
  }

  return maxConnected;
}

map<int, vector<int>> input(int n) {
  map<int, vector<int>> edges;

  int a, b;
  for (int i = 0; i < n; i += 1) {
    cin >> a >> b;
    auto it = edges.find(a);
    if (it == edges.end()) {
      vector<int> edgesFromA;
      edgesFromA.push_back(b);
      edges[a] = edgesFromA;
    } else {
      it->second.push_back(b);
      edges[a] = it->second;
    }

    it = edges.find(b);
    if (it == edges.end()) {
      vector<int> edgesFromB;
      edgesFromB.push_back(a);
      edges[b] = edgesFromB;
    } else {
      it->second.push_back(a);
      edges[b] = it->second;
    }
  }

  return edges;
}

int main() {
  int tests;
  cin >> tests;

  vector<int> results;
  for (int i = 0; i < tests; i += 1) {
    int rooms, hallways, newHallways;
    cin >> rooms >> hallways >> newHallways;

    auto edges = input(hallways);

    results.push_back(connected(rooms, newHallways, edges));
  }

  for (auto result : results) {
    cout << result << endl;
  }
}
