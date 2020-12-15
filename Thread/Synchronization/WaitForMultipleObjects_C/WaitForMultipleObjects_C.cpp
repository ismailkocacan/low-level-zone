#include <iostream>
#include <Windows.h>

#define EVENT_NAME L"KFLAG"

const int cThreadCount = 100;

int ActiveThreadCount = 0;
int CallbackFuncCallCount = 0;

HANDLE ThreadHandles[cThreadCount - 1];
HANDLE WaitObjects[cThreadCount - 1];


HANDLE hEventObject = 0;

VOID NTAPI WaitOrTimerCallback(PVOID lpParameter, BOOLEAN TimerOrWaitFired) {
    /*
    InterlockedAdd(&CallbackFuncCallCount, 1);
    if (InterlockedCompareExchange(CallbackFuncCallCount, CallbackFuncCallCount, cThreadCount) == cThreadCount) {
        InterlockedExchange(&CallbackFuncCallCount, 0);
        CloseHandle(hEventObject);
    }
    */
}


DWORD WINAPI ThreadStartFunction(LPVOID lpThreadParameter) {
    /*
    InterlockedAdd(&ActiveThreadCount, 1);
    HANDLE eventHandle = (HANDLE)lpThreadParameter;
    DWORD WaitResult = WaitForSingleObject(eventHandle, INFINITE);
    if (WaitResult == WAIT_OBJECT_0){
      InterlockedDecrement(&ActiveThreadCount);
    }
    */
    return 0;
}



int main()
{
    hEventObject = CreateEvent(NULL, true, false, EVENT_NAME);
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

        if (threadHandle) {
            HANDLE hWaitObject = 0;
            if (RegisterWaitForSingleObject(&hWaitObject,
                threadHandle,
                &WaitOrTimerCallback,
                &threadHandle,
                INFINITE,
                WT_EXECUTEONLYONCE))
                WaitObjects[i] = hWaitObject;
        }
    }

    std::cout << "Hello World!\n";
}
