#include <iostream>
#include <fstream>
#include <QCoreApplication>

typedef struct Node {
    int iValue;
    float fValue;
    char cValue[20];
    struct Node* prev;
    struct Node* next;
} *PNode;

using namespace std;

void writeFile(struct Node node){
    ofstream file;
    file.open("file.bin",ios::binary);
    file.write((char*)&node, sizeof(node));
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

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    struct Node node1 = { 31, 31.1f, "node1", NULL, NULL };
    struct Node node2 = { 32, 32.2f, "node2", NULL, NULL };
    struct Node node3 = { 33, 33.3f, "node2", NULL, NULL };

    node1.next = &node2;
    node2.prev = &node1;
    node2.next = &node3;
    node3.prev = &node2;
    writeFile(node1);

    struct Node node = {0};
    readFile(&node);

    PNode ptrNode = &node;
    while (ptrNode != nullptr){
       cout << ptrNode->cValue << " " << ptrNode->iValue << endl;
       ptrNode = ptrNode->next;
    }

    return a.exec();
}
