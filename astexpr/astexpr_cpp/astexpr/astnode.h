#ifndef ASTNODE_H
#define ASTNODE_H

#include <iostream>
using namespace std;

enum ASTNodeType
{
	Assignment = 0,
	Variable = 1,
	Expression = 2
};

class ASTNode
{
protected:
	ASTNodeType nodeType;
	ASTNode* left;
	ASTNode* right;
public:
	ASTNodeType GetNodeType();
	virtual ASTNode* GetLeft();
	virtual ASTNode* GetRight();
	void SetNodeType(ASTNodeType type);
	void SetLeft(ASTNode* left);
	void SetRight(ASTNode* right);
public:
	ASTNode();
	ASTNode(ASTNodeType type);
	virtual ~ASTNode();
};

typedef ASTNode* PASTNode;

#endif AST_H