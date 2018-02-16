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

typedef struct ASTExpressionNode{
   struct ASTNode astNode;
   enum ExpressionNodeType type;
   int value;
} *PASTExpressionNode;

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
    PASTExpressionNode left = (PASTExpressionNode)node->astNode.left;
    PASTExpressionNode right = (PASTExpressionNode)node->astNode.right;
    switch (node->type) {
        case PLUS: return evaluate(left) + evaluate(right);
        case MINUS: return evaluate(left) - evaluate(right);
        case DIV: return evaluate(left) / evaluate(right);
        case MUL: return evaluate(left) * evaluate(right);
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

    exprNodePlus->astNode.left = nodeValue_1;
    exprNodePlus->astNode.right = nodeMul;
    nodeMul->astNode.left = nodeValue_2;
    nodeMul->astNode.right = nodeValue_3;

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