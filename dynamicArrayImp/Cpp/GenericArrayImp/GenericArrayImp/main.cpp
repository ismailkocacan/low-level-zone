/*
 Author : isocan
 Purpose : How to implement dynamic array using pointer math and more generic.
*/
#include <iostream>

template <class Type>
class DynamicArray{
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
		// check out of range
		Type* p = (fMemBlock + (sizeof(Type) * index));
		return *p;
	}

	void SetElement(int index, Type value) {
		// check out of range
		Type* p = (fMemBlock + (sizeof(Type) * index));
		*p = value;
	}
};

int main() {

	DynamicArray<int> myArray(2);
	myArray.SetElement(0, 10);
	myArray.SetElement(1, 20);
	int value = myArray.GetElement(0);
	value = myArray.GetElement(1);

	std::cout << "hede hedöö" << std::endl;
	return 0;
}