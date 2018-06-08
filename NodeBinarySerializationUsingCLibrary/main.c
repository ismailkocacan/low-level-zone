#include <stdio.h>

typedef struct Node {
    int iValue;
    float fValue;
    char cValue[20];
    struct Node* prev;
    struct Node* next;
} Node, *PNode;

void writeFile(PNode node,size_t size){
   FILE* file = fopen("file.bin","wb");
   fwrite(node,size,1,file);
   fclose(file);
}

int getFileSize(FILE* file){
   int currentPos = 0;
   fseek(file, 0L, SEEK_END); // SetFilePointer ?
   currentPos = ftell(file);
   fseek(file, 0, SEEK_SET);
   return currentPos;
}

void readFile(PNode node){
   FILE* file = fopen("file.bin","rb");
   int fileSize = getFileSize(file);
   fread(node,fileSize,1,file);
   fclose(file);
}

int main() {

    Node node1 = { 31, 31.1f, "node1", NULL, NULL };
    Node node2 = { 32, 32.2f, "node2", NULL, NULL };
    Node node3 = { 33, 33.3f, "node2", NULL, NULL };
    node1.next = &node2;
    node2.prev = &node1;
    node2.next = &node3;
    node3.prev = &node2;
    writeFile(&node1, sizeof(node1));

    Node node = {0};
    readFile(&node);

    PNode ptrNode = &node;
    while (ptrNode != NULL){
        printf("%s %d \n",ptrNode->cValue,ptrNode->iValue);
        ptrNode = ptrNode->next;
    }

    return 0;
}