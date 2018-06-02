#include <Windows.h>

typedef int(*FunctionPtr)();

byte BYTE_CODE[] = { 0xB8, 0x04, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x03, 0xC3 };
const int CODE_SIZE = sizeof BYTE_CODE;

int main()
{
	PVOID P = VirtualAlloc(NULL, CODE_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	memcpy(P, BYTE_CODE, CODE_SIZE);
	FunctionPtr functionPtr = (FunctionPtr)P;
	VirtualFree(P, CODE_SIZE, MEM_RELEASE);
	int result = functionPtr();
}