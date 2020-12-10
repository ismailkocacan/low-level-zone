#include <Windows.h>
#include <iostream>

VOID NTAPI WaitOrTimerCallback(PVOID lpParameter, BOOLEAN TimerOrWaitFired){
    std::cout << "WaitOrTimerCallback" << std::endl;
}

DWORD WINAPI ThreadStartFunction(LPVOID lpThreadParameter) {
    std::cout << "ThreadStartFunction.begin" << std::endl;
    Sleep(3000);
    std::cout << "ThreadStartFunction.end" << std::endl;
    return 0;
}

int main()
{
    HANDLE hWaitObject = 0;
    DWORD lpThreadId = 0;
    HANDLE hThread = CreateThread(NULL, 
                                  0, 
                                  &ThreadStartFunction, 
                                  NULL, 
                                  0, 
                                  &lpThreadId);
    if (hThread) {
        RegisterWaitForSingleObject(&hWaitObject,
                                    hThread,
                                    &WaitOrTimerCallback,
                                    NULL,
                                    INFINITE, 
                                    WT_EXECUTEONLYONCE);

        WaitForSingleObject(hThread, INFINITE);
        //UnregisterWait(hWaitObject);
    }

    system("pause");
    std::cout << "Hello World!\n";
}