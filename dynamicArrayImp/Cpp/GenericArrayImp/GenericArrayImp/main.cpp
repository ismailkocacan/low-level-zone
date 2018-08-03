/*
 Author : isocan
 Purpose : How to implement dynamic array using pointer math and more generic.
*/
#include <iostream>
#include <string>
#include <windows.h>


typedef struct _Data {
	int value;
} Data, *PData;

template <class Type>
class DynamicArray{
private:
	int fLength;
	int fSize;
	Type* fMemBlock;
private:
	//https://www.wikiwand.com/en/Offset_(computer_science)
	int Offset(int index) {
		if (index < 0 || index > fLength) 
			throw std::runtime_error("IndexOutOfRangeException at "+ std::to_string(index));
		return sizeof(Type) * index;
	}
public:
	DynamicArray(size_t length) :
	    fLength(length) {

	   fSize = sizeof(Type) * fLength;
       #ifdef WIN32
		 fMemBlock = (Type*)VirtualAlloc(NULL, fSize, MEM_COMMIT, PAGE_READWRITE);
       #elif
		 fMemBlock = (Type*)malloc(sizeof(Type) * fLength);
       #endif
	}
	~DynamicArray() {
      #ifdef WIN32
		VirtualFree(fMemBlock, fSize, MEM_RELEASE);
      #elif
		free(fMemBlock);
      #endif
	}
public:
	Type GetElement(int index) {
		// check out of range
		Type* p = (fMemBlock + Offset(index));
		return *p;
	}

	Type* GetElementPointer(int index) {
		// check out of range
		Type* p = (fMemBlock + Offset(index));
		return p;
	}

	void SetElement(int index, Type value) {
		// check out of range
		Type* p = (fMemBlock + Offset(index));
		*p = value;
	}

	void SaveToFile(std::string filePath) {
		// to do
	}

	static DynamicArray LoadFromFile(std::string filePath) {
		// to do
	}
public:
	Type& operator[](int index) {
		Type* p = (fMemBlock + Offset(index));
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
	 

	try
	{
		DynamicArray<Data> dataArray(2);
		dataArray[0].value = 50;
		value = dataArray[0].value;
		PData dataPtr = dataArray.GetElementPointer(0);
		PData dataPtr2 = dataArray.GetElementPointer(30);
	}
	catch (const std::exception& e)
	{
		std::cout << e.what() << std::endl;
	}
	return 0;
}