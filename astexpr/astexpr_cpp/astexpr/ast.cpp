#include "ast.h"

ASTNode::ASTNode()
{
	this->type = ASTNodeType::UNDEFINED;
	this->value = 0;
	this->left = NULL;
	this->right = NULL;
}

ASTNode::~ASTNode()
{
	delete left;
	delete right;
}

ASTNodeType ASTNode::GetType()
{
	return this->type;
}

float ASTNode::GetValue()
{
	return this->value;
}

PASTNode ASTNode::GetLeft()
{
	return this->left;
}

PASTNode ASTNode::GetRight()
{
	return this->right;
}

void ASTNode::SetValue(float value)
{
	this->value = value;
}

void ASTNode::SetType(ASTNodeType type)
{
	this->type = type;
}

void ASTNode::SetLeft(ASTNode* left)
{
	this->left = left;
}

void ASTNode::SetRight(ASTNode* right)
{
	this->right = right;
}



bool IsNodeTypeEqual(PASTNode node, ASTNodeType nodeType)
{
	return node->GetType() == nodeType;
}


/*
   1 + 2 * 3 

	'+'
   /   \
 '1'    *
      /   \ 
    '2'   '3'

*/

float Evaluate(PASTNode node)
{
	if (node->GetValue() != NULL)
	{
		return node->GetValue();
	}

	if (IsNodeTypeEqual(node, ASTNodeType::PLUS))
	{
		return Evaluate(node->GetLeft()) + Evaluate(node->GetRight());
	}

	if (IsNodeTypeEqual(node, ASTNodeType::MINUS))
	{
		return Evaluate(node->GetLeft()) - Evaluate(node->GetRight());
	}

	if (IsNodeTypeEqual(node, ASTNodeType::DIV))
	{
		// check division by zero ?
		return Evaluate(node->GetLeft()) / Evaluate(node->GetRight());
	}

	if (IsNodeTypeEqual(node, ASTNodeType::MUL))
	{
		return Evaluate(node->GetLeft()) * Evaluate(node->GetRight());
	}
}