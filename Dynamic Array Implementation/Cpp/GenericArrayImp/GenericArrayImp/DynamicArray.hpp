/*
Author : isocan
Purpose : How to implement dynamic array using pointer math and more generic.
*/

#ifndef DYNAMICARRAY_H
#define DYNAMICARRAY_H

#include <iostream>
#include <fstream>
#include <string>
#ifdef WIN32
#include <windows.h>
#endif

typedef struct _PageRange {
	int StartIndex;
	int EndIndex;
} PageRange, *PPageRange;

class Paging {
private:
	int fPageCount;
	int fCount;
	int fPageSize;
private:
	int GetStartPageIndex(int pageNo) {
		return (pageNo * fPageSize) - fPageSize;
	}
	int GetEndPageIndex(int pageNo) {
		if (pageNo % fPageSize != 0 && pageNo == fPageCount) {
			return fCount - 1;
		}
		return (GetStartPageIndex(pageNo) + fPageSize) - 1;
	}
public:
	int GetPageCount(int count, int pageSize) {
		fCount = count;
		fPageSize = pageSize;
		if (count % pageSize == 0)
			fPageCount = count / pageSize;
		else
			fPageCount = (count / pageSize) + 1;
		return fPageCount;
	}
	PageRange GetPageRange(int pageNo) {
		PageRange range;
		range.StartIndex = GetStartPageIndex(pageNo);
		range.EndIndex = GetEndPageIndex(pageNo);
	}
};

template <class Type>
class DynamicArray {
private:
	int fLength;
	int fSize;
	Type* fMemBlock;
	bool fRangeChecking;
private:
	//https://www.wikiwand.com/en/Offset_(computer_science)
	int Offset(int index) {
		if (fRangeChecking) {
		   if (index < 0 || index > fLength - 1)
			  throw std::runtime_error("IndexOutOfRangeException at " + std::to_string(index));
		}
		return sizeof(Type) * index;
	}
public:
	DynamicArray(size_t length) :
		fLength(length) {
		RangeChecking(true);
		fSize = sizeof(Type) * fLength;
        #ifdef WIN32
		  fMemBlock = (Type*)VirtualAlloc(NULL, fSize, MEM_COMMIT, PAGE_READWRITE);
        #else
		  fMemBlock = (Type*)malloc(sizeof(Type) * fLength);
        #endif
	}

	DynamicArray(std::string filePath) {
		RangeChecking(true);
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
        #else
		   fMemBlock = (Type*)malloc(sizeof(Type) * fLength);
        #endif
		file.read((char*)&fMemBlock, fSize);
		file.close();
	}

	~DynamicArray() {
        #ifdef WIN32
		  VirtualFree(fMemBlock, fSize, MEM_RELEASE);
        #else
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

	void RangeChecking(bool value) {
		fRangeChecking = value;
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
	Type & operator[](int index) {
		Type* p = (fMemBlock + Offset(index));
		return *p;
	}
};
#endif