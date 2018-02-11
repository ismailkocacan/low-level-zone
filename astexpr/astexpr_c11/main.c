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

int i = 0;

typedef void* Pointer;

Pointer pointers[1024];

enum ASTNodeType{
    Assignment = 0,
    Variable = 1,
    Expression = 2,
};
enum ExpressionNodeType{
    UNDEFINED = 0,
    PLUS = 1,
    MINUS = 2,
    DIV = 3,
    MUL = 4
};

typedef struct ASTNode{
   Pointer left;
   Pointer right;
   enum ASTNodeType nodeType;
} *PASTNode;

typedef struct ASTExpressionNode{
   struct ASTNode astNode;
   enum ExpressionNodeType type;
   struct ASTExpressionNode* left;
   struct ASTExpressionNode* right;
   int value;
} *PASTExpressionNode;

typedef struct ASTAssignmentNode{
   struct ASTNode astNode;
} *PASTAssignmentNode;

typedef struct ASTVariableNode{
   struct ASTNode astNode;
   char variableName[20];
} *PASTVariableNode;

typedef struct NodeX{
   Pointer node;
   enum ASTNodeType nodeType;
} *PNodeX;

Pointer new(size_t size){
   Pointer  p = malloc(size);
   pointers[i] = p;
   i++;
   return p;
};

void delete(Pointer ptr){
   free(ptr);
}

Pointer createNode(enum ASTNodeType type){
    switch (type){
        case Assignment: {
            PASTAssignmentNode node = new(sizeof(struct ASTAssignmentNode));
            node->astNode.nodeType = Assignment;
            return node;
        }
        case Expression: {
            PASTExpressionNode node = new(sizeof(struct ASTExpressionNode));
            node->value = 0;
            node->type = UNDEFINED;
            node->astNode.nodeType = Expression;
            return node;
        }
        case Variable: {
            PASTVariableNode node = new(sizeof(struct ASTVariableNode));
            node->astNode.nodeType = Variable;
            return node;
        }
        default:{
            return NULL;
        }
    }
};

float evaluate(PASTExpressionNode node){
    if (node->value != 0) return node->value;
    switch (node->type) {
        case PLUS: return evaluate(node->left) + evaluate(node->right);
        case MINUS: return evaluate(node->left) - evaluate(node->right);
        case DIV: return evaluate(node->left) / evaluate(node->right);
        case MUL: return evaluate(node->left) * evaluate(node->right);
        default:{
            return 0.0f;
        }
    }
}

float interpret(PNodeX node) {
    switch (node->nodeType){
        case Assignment:{
            PASTAssignmentNode assignmentNode =  (PASTAssignmentNode)node->node;
            PASTVariableNode variableNode = ((PASTVariableNode)assignmentNode->astNode.left);
            PASTExpressionNode  expressionNode = ((PASTExpressionNode)assignmentNode->astNode.right);
            // ??
            return evaluate(expressionNode);
        }
        case Expression:{
            //struct ASTExpressionNode expressionNode = (*(PASTExpressionNode)node->node);
            PASTExpressionNode expressionNode =  (PASTExpressionNode)node->node;
            return evaluate(expressionNode);
        }
        default:{
            return 0.0f;
        }
    }
}

int main() {
    PASTExpressionNode exprNodePlus = createNode(Expression);
    exprNodePlus->type = PLUS;
    PASTExpressionNode nodeValue_1  = createNode(Expression);
    nodeValue_1->value = 1;

    PASTExpressionNode nodeMul = createNode(Expression);
    nodeMul->type = MUL;
    PASTExpressionNode nodeValue_2  = createNode(Expression);
    nodeValue_2->value = 2;
    PASTExpressionNode nodeValue_3  = createNode(Expression);
    nodeValue_3->value = 3;

    exprNodePlus->left = nodeValue_1;
    exprNodePlus->right = nodeMul;
    nodeMul->left = nodeValue_2;
    nodeMul->right = nodeValue_3;

    PASTAssignmentNode assignmentNode = createNode(Assignment);
    PASTVariableNode variableNode = createNode(Variable);
    strcpy(variableNode->variableName,"x");
    assignmentNode->astNode.left = variableNode;
    assignmentNode->astNode.right =  exprNodePlus;

    float result = evaluate(exprNodePlus);
    printf("result : %f",result);

    struct NodeX node1 = { exprNodePlus, Expression };
    float result1 = interpret(&node1);
    printf("result1 : %f",result1);

    struct NodeX node2 = { assignmentNode, Assignment };
    float result2 = interpret(&node2);
    printf("result2 : %f",result2);

    int count = sizeof(pointers) / sizeof(Pointer);
    for (int j = 0; j < count; j++) {
        if (!pointers[j]) break;
        delete(pointers[j]);
        pointers[j] = NULL;
    }
    return 0;
}