#include <stdio.h>
#include <Windows.h>

#define EVENT_NAME L"Test"

DWORD WINAPI ThreadStartFunction(LPVOID lpThreadParameter) {
    // Belki NULL gönderdi. Olamaz mý olabilir...
    if (lpThreadParameter == NULL)
        return 0;

    HANDLE eventHandle = (HANDLE)lpThreadParameter;
    while (true) {
        printf("WaitForSingleObject.... \n");
        DWORD eventResult = WaitForSingleObject(eventHandle, INFINITE);
        if (eventResult == WAIT_OBJECT_0) {
            /*
             
            */
            printf("Sinyal geldi. \n");
            ResetEvent(eventHandle);
        }
    }
    return 0;
}

int main()
{
    int operation = 0;
    printf("Op Code Girin: [CreateEvent=1 OpenEvent=2]:");
    scanf_s("%d", &operation);
    
    if (operation == 1) {
        HANDLE eventHandle = CreateEvent(NULL,
                                         false,
                                         false,
                                         EVENT_NAME);
        if (eventHandle == 0) {
            printf("GetLastError:%d", GetLastError());
            return EXIT_FAILURE;
        }
        DWORD lpThreadId = 0;
        HANDLE threadHandle = CreateThread(NULL,
                                            0,
                                            &ThreadStartFunction,
                                            eventHandle,
                                            0,
                                            &lpThreadId);
        if (threadHandle != 0) {
            WaitForSingleObject(threadHandle, INFINITE);
            CloseHandle(threadHandle);
        }
        if (eventHandle != 0)
            CloseHandle(eventHandle);
    }



    if (operation == 2) {
        HANDLE eventHandle = OpenEvent(EVENT_ALL_ACCESS, 
                                       true, 
                                       EVENT_NAME);
        if (eventHandle != 0) {
            while (true) { 
                printf("Set etmek istiyor musun? E/H:");
                char input;
                scanf_s("%c", &input);
                if (strcmp(&input, "E")) {
                    SetEvent(eventHandle);
                }
            }
        }
    }

    printf("Bitti \n");
    return EXIT_SUCCESS;
}