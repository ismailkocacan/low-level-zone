#include "expressionnode.h"

ExpressionNode::ExpressionNode(int value) :
	ASTNode(ASTNodeType::Expression),
	type(ExpressionNodeType::UNDEFINED),
	value(value)
{

}

ExpressionNode::ExpressionNode(ExpressionNodeType type) :
	ASTNode(ASTNodeType::Expression),
	type(type),
	value(0)
{

}

ExpressionNode::~ExpressionNode()
{
	cout << "ExpressionNode destructor" << endl;
}

int ExpressionNode::GetValue()
{
	return this->value;
}

ExpressionNodeType ExpressionNode::GetType()
{
	return this->type;
}