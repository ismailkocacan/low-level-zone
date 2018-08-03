/*
 Author : isocan
 Purpose : How to implement dynamic array using pointer math and more generic.
*/
#include <iostream>
#include <fstream>
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

	DynamicArray(std::string filePath){
		int typeSize = 0;
		int fileSize = 0;
		std::ifstream file;
		file.open(filePath, std::ios::binary);
		file.seekg(0, std::ios::end);
		fileSize = (int)file.tellg();
		file.seekg(0, std::ios::beg);
		file.read((char*)&typeSize, sizeof(int));
		
		fSize = fileSize - typeSize;
		fLength = fSize / typeSize;
		
		file.seekg(sizeof(int));
        #ifdef WIN32
		   fMemBlock = (Type*)VirtualAlloc(NULL, fSize, MEM_COMMIT, PAGE_READWRITE);
        #elif
		   fMemBlock = (Type*)malloc(sizeof(Type) * fLength);
        #endif
		file.read((char*)&fMemBlock, fSize);
		file.close();
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
		Type* p = (fMemBlock + Offset(index));
		return *p;
	}

	Type* GetElementPointer(int index) {
		Type* p = (fMemBlock + Offset(index));
		return p;
	}

	void SetElement(int index, Type value) {
		Type* p = (fMemBlock + Offset(index));
		*p = value;
	}

	int GetLength() {
		return fLength;
	}

	int GetSize() {
		return fSize;
	}

	void Serialize(std::string filePath) {
		std::ofstream file;
		file.open(filePath.c_str(), std::ios::binary);
		int typeSize = sizeof(Type); // first 4 byte is size of data type.
		file.write((char*)&typeSize, sizeof(int)); 
		file.write((char*)&fMemBlock, fSize);
		file.close();
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
		dataArray[1].value = 36;
		dataArray.Serialize("data_array.bin");

		DynamicArray<int> dataArray2("data_array.bin");
		int test = dataArray2.GetElement(0);
		int test2 = dataArray2.GetElement(1);

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