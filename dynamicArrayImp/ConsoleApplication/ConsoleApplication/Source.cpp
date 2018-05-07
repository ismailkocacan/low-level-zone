#include <cstdio>
#include <Windows.h>

PINT pMemBlock = NULL;
void CreateArray(int count)
{
	size_t size = sizeof(int) * count;
	pMemBlock = (PINT)malloc(size);
}

void SetArrayElement(int index, int value)
{
	// check out of range 
	PINT p = (pMemBlock + (sizeof(int) * index));
	*p = value;
}

int GetArrayElement(int index) 
{
	// check out of range 
	PINT p = (pMemBlock + (sizeof(int) * index));
	return *p;
}

void DeleteArray()
{
	if (!pMemBlock) return;
	free(pMemBlock);
}

int main()
{
	int value = 0;
	int count = 4; 
	CreateArray(count);
	SetArrayElement(0, 11); 
	SetArrayElement(1, 22); 
	SetArrayElement(2, 33); 
	SetArrayElement(3, 34); 
	for (int i = 0; i < count; i++)
	{
		value = GetArrayElement(i);
		printf("element : %d\n", value);
	}
	DeleteArray();
}