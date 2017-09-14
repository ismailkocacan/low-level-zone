#include "astnode.h"
#include <stdio.h>
#include <iostream>
using namespace std;


ASTNode::ASTNode()
{

}

ASTNode::ASTNode(ASTNodeType type) :
	nodeType(type),
	left(nullptr),
	right(nullptr)
{

}

ASTNode::~ASTNode()
{
	cout << "ASTNode destructor" << endl;
	cout << "-------------------------" << endl;

	if (left != nullptr) delete left;
	if (right != nullptr) delete right;

	// review later.
	/*
	if (left != (void*)0xcdcdcdcd) delete left;
	if (right != (void*)0xcdcdcdcd) delete right;
	*/
}

ASTNodeType ASTNode::GetNodeType()
{
	return this->nodeType;
}

PASTNode ASTNode::GetLeft()
{
	return this->left;
}

PASTNode ASTNode::GetRight()
{
	return this->right;
}

void ASTNode::SetNodeType(ASTNodeType type)
{
	this->nodeType = type;
}

void ASTNode::SetLeft(ASTNode* left)
{
	this->left = left;
}

void ASTNode::SetRight(ASTNode* right)
{
	this->right = right;
}