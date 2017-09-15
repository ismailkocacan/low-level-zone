#ifndef EXPRESSIONNODE_H
#define EXPRESSIONNODE_H

#include "astnode.h"
#include <iostream>
using namespace std;

enum ExpressionNodeType
{
	UNDEFINED = 0,
	PLUS = 1,
	MINUS = 2,
	DIV = 3,
	MUL = 4
};

class ExpressionNode :
	public ASTNode
{   
private:
	int value;
	ExpressionNodeType type;
public:
	int GetValue();
	ExpressionNodeType GetType();
public:
	ExpressionNode(int value);
	ExpressionNode(ExpressionNodeType type);
	virtual ~ExpressionNode();
};

typedef ExpressionNode* PExpressionNode;

#endif
