#ifndef VARIABLENODE_H
#define VARIABLENODE_H

#include "astnode.h"
#include <iostream>

using namespace std;

class VariableNode :
	public ASTNode
{
private:
	string name;
public:
	string GetName();
public:
	VariableNode(string name);
	virtual ~VariableNode();
};

typedef VariableNode* PVariableNode;

#endif
