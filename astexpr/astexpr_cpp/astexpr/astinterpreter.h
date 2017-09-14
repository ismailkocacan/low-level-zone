/*

 1 + 2 * 3

   '+'
  /   \
'1'    *
     /   \
   '2'   '3'

*/


#ifndef ASTINTERPRETER_H
#define ASTINTERPRETER_H

#include "astnode.h"
#include "variablenode.h"
#include "expressionnode.h"
#include "assignmentnode.h"
#include <iostream>
#include <map>

using namespace std;

class ASTInterpreter
{
private:
	PASTNode root;
	map<string,float> hashTable;
private:
	float Evaluate(PExpressionNode node);
	bool IsNodeTypeEqual(PExpressionNode node, ExpressionNodeType nodeType);
public:
	float Interpret();
public:
	ASTInterpreter(PASTNode node);
	~ASTInterpreter();
};

#endif