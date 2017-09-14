#include "variablenode.h"


VariableNode::VariableNode(string name) :
	ASTNode(ASTNodeType::Variable),
	name(name)
{

}

VariableNode::~VariableNode()
{
	cout << "VariableNode destructor" << endl;
}

string VariableNode::GetName()
{
	return this->name;
}

