/*
 Author : isocan
 Purpose : How to implement dynamic array using pointer math and more generic.
*/
#include <iostream>

typedef struct _Data {
	int value;
} Data, *PData;

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

	Type* GetElementPointer(int index) {
		// check out of range
		Type* p = (fMemBlock + (sizeof(Type) * index));
		return p;
	}

	void SetElement(int index, Type value) {
		// check out of range
		Type* p = (fMemBlock + (sizeof(Type) * index));
		*p = value;
	}
public:
	Type& operator[](int index) {
		Type* p = (fMemBlock + (sizeof(Type) * index));
		return *p;
	}
};

int main() {

	DynamicArray<int> myArray(2);
	myArray.SetElement(0, 10);
	myArray.SetElement(1, 20);
	int value = myArray.GetElement(0);
	value = myArray.GetElement(1);
	value = *myArray.GetElementPointer(1);

	myArray[0] = 31;
	myArray[1] = 32;
	value = myArray[0];
	value = myArray[1];
	

	DynamicArray<Data> dataArray(2);
	dataArray[0].value = 50;
	value = dataArray[0].value;
	PData dataPtr = dataArray.GetElementPointer(0);
	
	return 0;
}