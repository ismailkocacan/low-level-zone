#include <stdio.h>
#include "dynamicarray.h"

#ifdef _WIN32
#include <tchar.h>
#else
typedef char _TCHAR;
#define _tmain main
#endif

int _tmain(int argc, _TCHAR* argv[]) {

	int i;
	int value = 0;
	int count = 4;
	PArray myArray = NewArray(count);
	SetArrayElement(myArray, 0, 11);
	SetArrayElement(myArray, 1, 22);
	SetArrayElement(myArray, 2, 33);
	SetArrayElement(myArray, 3, 34);
	for (i = 0; i < count; i++) {
		value = GetArrayElement(myArray, i);
		printf("element : %d\n", value);
	}
	DeleteArray(myArray);
	return 0;
}
