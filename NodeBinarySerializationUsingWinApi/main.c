#include <stdio.h>
#include <windows.h>

typedef struct Node {
    int iValue;
    float fValue;
    char cValue[20];
    struct Node* prev;
    struct Node* next;
} *PNode;

int main() {

    struct Node node1 = { 31, 31.1f, "node1", NULL, NULL };
    struct Node node2 = { 32, 32.2f, "node2", NULL, NULL };
    struct Node node3 = { 33, 33.3f, "node2", NULL, NULL };
    node1.next = &node2;
    node2.prev = &node1;
    node2.next = &node3;
    node3.prev = &node2;

    HANDLE hFile = CreateFile("file.bin",
                   GENERIC_WRITE, 0,
                   NULL,
                   CREATE_NEW,
                   FILE_ATTRIBUTE_NORMAL,
                   NULL);
    DWORD lpNumberOfBytesWritten;
    WriteFile(hFile, &node1, sizeof(node1),
                    &lpNumberOfBytesWritten, NULL);
    CloseHandle(hFile);

    struct Node node = {0};
    HANDLE hFile2 = CreateFile("file.bin",
                     GENERIC_READ, 0,
                     NULL,
                     OPEN_EXISTING,
                     FILE_ATTRIBUTE_NORMAL,
                     NULL);
    DWORD lpNumberOfBytesRead;
    ReadFile(hFile2, &node,
                sizeof(struct Node),
                &lpNumberOfBytesRead, NULL);
    CloseHandle(hFile2);

    PNode ptrNode = &node;
    while (ptrNode != NULL){
        printf("%s %d \n",ptrNode->cValue,ptrNode->iValue);
        ptrNode = ptrNode->next;
    }
    return 0;
}