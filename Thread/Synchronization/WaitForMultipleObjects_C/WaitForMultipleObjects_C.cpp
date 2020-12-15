#include <iostream>
#include <Windows.h>

#define EVENT_NAME L"KFLAG"

const int cThreadCount = 100;
HANDLE ThreadHandles[cThreadCount - 1];
HANDLE WaitObjects[cThreadCount - 1];

VOID NTAPI WaitOrTimerCallback(PVOID lpParameter, BOOLEAN TimerOrWaitFired) {

}


DWORD WINAPI ThreadStartFunction(LPVOID lpThreadParameter) {
    return 0;
}



int main()
{
    HANDLE hEventObject = CreateEvent(NULL, true, false, EVENT_NAME);
    if (hEventObject == 0)
        return 0;
    ResetEvent(hEventObject);

    int length = sizeof(ThreadHandles) / sizeof(ThreadHandles[0]);
    for (size_t i = 0; i < length; i++)
    {
        DWORD lpThreadId = 0;
        HANDLE threadHandle = CreateThread(NULL,
                                            0,
                                            &ThreadStartFunction,
                                            hEventObject,
                                            0,
                                            &lpThreadId);
        ThreadHandles[i] = threadHandle;

        HANDLE hWaitObject = 0;
        if (RegisterWaitForSingleObject(&hWaitObject,
                                        threadHandle,
                                        &WaitOrTimerCallback,
                                        &threadHandle,
                                        INFINITE,
                                        WT_EXECUTEONLYONCE))
            WaitObjects[i] = hWaitObject;
    }

    std::cout << "Hello World!\n";
}
