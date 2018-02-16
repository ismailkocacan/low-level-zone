/*
   1 + 2 * 3
	'+'
   /   \
 '1'    *
      /   \
    '2'   '3'
-----------------------------------
1 + 2 * 3      => expression
x              => variable
x = 1 + 2 * 3  => assignment statement
  assignment node
   /            \
 variable       expression
  x                	'+'
                   /   \
                 '1'    *
                      /   \
                    '2'   '3'
-----------------------------------
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void* Pointer;
typedef int* PInt;

enum ASTNodeType{
    Assignment = 0,
    Variable = 1,
    Operator = 2,
    Value = 3
};

enum ASTOperatorType{
    UnDefined = 0,
    Plus = 1,
    Minus = 2,
    Div = 3,
    Mul = 4
};

typedef struct ASTNode{
   Pointer data;
   struct ASTNode* left;
   struct ASTNode* right;
   enum ASTNodeType nodeType;
} *PASTNode;

typedef struct ASTOperatorNode{
    enum ASTOperatorType value;
} *PASTOperatorNode;

typedef struct ASTVariableNode{
   char variableName[20];
} *PASTVariableNode;

Pointer new(size_t size){
   Pointer  p = malloc(size);
   return p;
};

void delete(Pointer ptr){
   free(ptr);
}

Pointer createNode(enum ASTNodeType type){
    PASTNode node = new(sizeof(struct ASTNode));
    node->data = NULL;
    node->left = NULL;
    node->right = NULL;
    node->nodeType = type;
    return node;
};

float evaluate(PASTNode node){
    if (!node) return 0.0f;
    switch(node->nodeType){
        case Operator:{
            PASTOperatorNode operatorTypeData = (PASTOperatorNode)node->data;
            switch (operatorTypeData->value) {
                case Plus: return evaluate(node->left) + evaluate(node->right);
                case Minus: return evaluate(node->left) - evaluate(node->right);
                case Div: return evaluate(node->left) / evaluate(node->right);
                case Mul: return evaluate(node->left) * evaluate(node->right);
                default:{
                    return 0.0f;
                }
            }
        }
        case Value:{
            int value = (*(PInt)node->data);
            if (value != 0) return value;
        }
    }
}

float interpret(PASTNode node) {
    switch (node->nodeType){
        case Assignment:{
            PASTNode nodeVariable = node->left;
            PASTNode nodeOperator = node->right;
            return evaluate(nodeOperator);
        }
        case Operator:{
            return evaluate(node);
        }
        default:{
            return 0.0f;
        }
    }
}

void freeNode(PASTNode node){
    if (node){
        freeNode(node->left);
        freeNode(node->right);
        if (node->data) delete(node->data);
        node->data = NULL;
        delete(node);
        node = NULL;
    }
}

int main() {
    PASTNode nodeAssignment,
            nodeVariable,
            nodeOperatorPlus,
            nodeOperatorMul,
            nodeValue_1,
            nodeValue_2,
            nodeValue_3;

    nodeAssignment = createNode(Assignment);

    nodeVariable = createNode(Variable);
    PASTVariableNode varNode = new(sizeof(struct ASTVariableNode));
    strcpy(varNode->variableName,"x");
    nodeVariable->data = varNode;

    nodeOperatorPlus = createNode(Operator);
    PASTOperatorNode opDataPlus = new(sizeof(struct ASTOperatorNode));
    opDataPlus->value = Plus;
    nodeOperatorPlus->data = opDataPlus;

    nodeOperatorMul = createNode(Operator);
    PASTOperatorNode opDataMul = new(sizeof(struct ASTOperatorNode));
    opDataMul->value = Mul;
    nodeOperatorMul->data = opDataMul;

    nodeValue_1 = createNode(Value);
    nodeValue_1->data = new(sizeof(int));
    (*(PInt)nodeValue_1->data) = 1;

    nodeValue_2 = createNode(Value);
    nodeValue_2->data = new(sizeof(int));
    (*(PInt)nodeValue_2->data) = 2;

    nodeValue_3 = createNode(Value);
    nodeValue_3->data = new(sizeof(int));
    (*(PInt)nodeValue_3->data) = 3;

    nodeOperatorPlus->left  = nodeValue_1;
    nodeOperatorPlus->right = nodeOperatorMul;
    nodeOperatorMul->left = nodeValue_2;
    nodeOperatorMul->right = nodeValue_3;

    nodeAssignment->left = nodeVariable;
    nodeAssignment->right = nodeOperatorPlus;

    float result = evaluate(nodeOperatorPlus);
    printf("result : %f",result);

    float result1 = interpret(nodeAssignment);
    printf("result1 : %f",result1);

    freeNode(nodeAssignment);

    return 0;
}