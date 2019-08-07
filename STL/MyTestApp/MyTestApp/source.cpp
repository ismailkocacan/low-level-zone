#include <vector>
#include <cstdint>
#include <iostream>
#include <algorithm>

int main() {
	std::vector<int32_t> vect;
	for (size_t i = 0; i < 1000000; i++)
		vect.push_back(i);
	//const auto [min, max] = std::minmax_element(std::begin(vect), std::end(vect));
	const auto [min, max] = std::minmax_element(vect.begin(), vect.end());
	std::for_each(vect.begin(), vect.end(), [](int32_t& i) {
		std::cout << i << std::endl;
	});
	
	//typedef std::vector<int32_t>::const_iterator Firildak;
	using Firildak = std::vector<int32_t>::const_iterator;
	Firildak baslangic = vect.cbegin();
	Firildak bitis = vect.cend();
	while (baslangic != bitis) {
		std::cout << *baslangic << std::endl;
		baslangic++;
	}
	return EXIT_SUCCESS;
}