/*
 Author  : isocan
 Purpose : How to implement dynamic array using pointer math.
*/

#ifndef _DYNAMIC_ARRAY
#define _DYNAMIC_ARRAY

#include <stdlib.h>

typedef int* PArray;

PArray NewArray(int count);

void SetArrayElement(PArray anArray, int index, int value);

int GetArrayElement(PArray anArray, int index);

void DeleteArray(PArray anArray);

#endif