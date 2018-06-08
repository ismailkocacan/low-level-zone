#include <iostream>
#include <fstream>
using namespace std;

typedef struct Node {
    int iValue;
    float fValue;
    char cValue[20];
    Node* prev;
    Node* next;
} Node, *PNode;

void writeFile(PNode node,size_t size){
    ofstream file;
    file.open("file.bin",ios::binary);
    file.write((char*)node, size);
    file.close();
}

int getFileSize(ifstream &file){
    file.seekg(0,ios::end);
    int currentPos = (int)file.tellg();
    file.seekg(0,ios::beg);
    return currentPos;
}

void readFile(PNode node){
    ifstream file;
    file.open("file.bin",ios::binary);
    int fileSize = getFileSize(file);
    file.read((char*)node,fileSize);
    file.close();
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
    while (ptrNode != nullptr){
        cout << ptrNode->cValue << " " << ptrNode->iValue << endl;
        ptrNode = ptrNode->next;
    }
    return 0;
}