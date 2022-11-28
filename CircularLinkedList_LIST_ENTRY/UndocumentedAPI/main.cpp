#include <stdio.h>
#include <Windows.h>
#include <winnt.h>
#include <Winternl.h>

int main()
{
	PTEB teb = NtCurrentTeb();
	PLIST_ENTRY root = &teb->ProcessEnvironmentBlock->Ldr->InMemoryOrderModuleList;

	PLIST_ENTRY current;

	current = root->Flink;
	while (current->Flink != root)
	{
		LDR_DATA_TABLE_ENTRY entry = *(PLDR_DATA_TABLE_ENTRY)current;
		wprintf(entry.FullDllName.Buffer);
		wprintf(L"\n");
		current = current->Flink;
	}


	PLIST_ENTRY last = root->Blink;
	current = last;
	while (current->Blink != current)
	{
		LDR_DATA_TABLE_ENTRY entry = *(PLDR_DATA_TABLE_ENTRY)current;
		wprintf(entry.FullDllName.Buffer);
		wprintf(L"\n");
		current = current->Blink;
	}
}