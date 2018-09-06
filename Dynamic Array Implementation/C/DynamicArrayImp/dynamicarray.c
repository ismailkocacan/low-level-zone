#include "dynamicarray.h"

PArray NewArray(int count) {
	size_t size = sizeof(int) * count;
	return malloc(size);
}

int Offset(int index) {
	return sizeof(int) * index;
}

void SetArrayElement(PArray anArray, int index, int value) {
	// check out of range
	int* p = (anArray + Offset(index));
	*p = value;
}

int GetArrayElement(PArray anArray, int index) {
	// check out of range
	int* p = (anArray + Offset(index));
	return *p;
}

void DeleteArray(PArray anArray) {
	free(anArray);
}
