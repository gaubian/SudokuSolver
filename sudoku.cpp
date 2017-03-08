#include <iostream>
#include <vector>
#include <stack>
#include <set>

using matrix = std::vector<std::vector<int>>;

bool okay_zone(std::vector<int> &zone) {
	std::set<int> seen;
	int zeros = 0;
	for(int i : zone) {
		if(i == 0) zeros++;
		else seen.insert(i);
	}
	return seen.size() + zeros == 9;
}

bool possible(matrix &mat, int i, int j) {
	std::vector<int> line(9);
	std::vector<int> column(9);
	std::vector<int> square(9);
	int ii = 3 * (i / 3);
	int jj = 3 * (j / 3);
	for(int k = 0; k < 9; ++k) {
		line[k] = mat[i][k];
		column[k] = mat[k][j];
		square[k] = mat[ii + (k % 3)][jj + (k / 3)];
	}
	return okay_zone(line) && okay_zone(column) && okay_zone(square);
}

int main() {
	matrix mat(9,std::vector<int> (9,0));
	char c;
	std::stack<std::pair<int,int>> seen;
	std::stack<std::pair<int,int>> rem;
	for(int i = 0; i < 9; ++i) {
	for(int j = 0; j < 9; ++j) {
		std::cin >> c;
		if(c != '0') mat[i][j] = c - '0';
		else rem.push({i,j});
	}
	}
	while(!rem.empty()) {
		std::pair<int,int> x = rem.top();
		int i = x.first, j = x.second;
		if(mat[i][j] == 9) {
			mat[i][j] = 0;
			if(seen.empty()) break;
			std::pair<int,int> y = seen.top();
			seen.pop();
			rem.push(y);
		}
		else {
			mat[i][j]++;
			if(possible(mat,i,j)) {
				rem.pop();
				seen.push(x);
			}
		}

	}
	if(rem.empty()) {
		for(int i = 0; i < 9; ++i) {
			for(int j = 0; j < 9; ++j) std::cout << mat[i][j];
			std::cout << std::endl;
		}
	}
	else std::cout << "NO SOLUTION" << std::endl;
}