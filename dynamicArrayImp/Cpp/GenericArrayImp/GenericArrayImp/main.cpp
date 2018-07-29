
#include <iostream>

template <class Type>
class DynamicArray
{
private:
	int fLength;
	Type* fMemBlock;
public:
	DynamicArray(size_t length) :
		fLength(length) {
		fMemBlock = (Type*)malloc(sizeof(Type) * fLength);
	}
	~DynamicArray() {
		free(fMemBlock);
	}
public:
	Type GetElement(int index) {

	}

	Type SetElement(int index) {

	}
};

int main() {
	DynamicArray<int> myArray(2);
	std::cout << "hede hedöö" << std::endl;
	return 0;
}