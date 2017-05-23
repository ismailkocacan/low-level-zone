#include <stdio.h>
#include <iostream>
#include <fstream>
#include <Windows.h>

using namespace std;

void LogError()
{
	wchar_t buf[256];
	FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), buf, 256, NULL);
	wprintf(buf);
}

void CreateSampleTestData(string filePath)
{
	ofstream file(filePath);
	for (int i = 0; i <= 30000000; i++)
	{
		file << "Test Data " << i << endl;
	}
	file.close();
	Beep(500, 1000);
}

int main(int argc, char *argv[])
{
	char filePath[MAX_PATH];
	strcpy_s(filePath, argv[1]);


	if (GetFileAttributes(filePath) == INVALID_FILE_ATTRIBUTES)
		CreateSampleTestData(filePath);


	HANDLE fileHandle = CreateFile(filePath,
		GENERIC_READ,
		0,
		NULL,
		OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,
		NULL);
	if (fileHandle == INVALID_HANDLE_VALUE)
	{
		LogError();
		return 1;
	}

	LARGE_INTEGER lpFileSize;
	if (!GetFileSizeEx(fileHandle, &lpFileSize))
	{
		LogError();
		return 1;
	}
	DWORD fileSize = lpFileSize.LowPart;


	HANDLE fileMapHandle = CreateFileMapping(fileHandle, NULL, PAGE_READONLY, 0, 0, NULL);
	if (fileMapHandle == 0)
	{
		LogError();
		return 1;
	}

	PCHAR baseAddress = (PCHAR)MapViewOfFile(fileMapHandle, FILE_MAP_READ, 0, 0, fileSize);
	if (baseAddress == NULL)
	{
		LogError();
		return 1;
	}


	char line[1024];
	PCHAR current, last;

	current = baseAddress;
	last = current + fileSize;

	PCHAR begin = current;
	PCHAR p = current;
	size_t charCount = 0;


	DWORD start, stop;
	start = GetTickCount();

	while ((p < last) && (*p != EOF))
	{
		if (*p == '\n')
		{
			charCount = p - begin;
			if (charCount)
			{
				memcpy(line, begin, charCount);
				line[charCount] = 0;
				begin = p + 1;
			}
		}
		p++;
	}

	stop = GetTickCount();
	DWORD diff = stop - start;
	cout << diff << " ms" << endl;

	MessageBox(0, line, "", 0);

	UnmapViewOfFile(baseAddress);
	CloseHandle(fileHandle);
	return 0;
}