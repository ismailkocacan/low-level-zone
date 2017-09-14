#include "astinterpreter.h"


ASTInterpreter::ASTInterpreter(PASTNode node):
	root(node)
{

}

ASTInterpreter::~ASTInterpreter()
{
	hashTable.clear();
	if (root != nullptr) delete root;
}

float ASTInterpreter::Evaluate(PExpressionNode node)
{
	if (node->GetValue() != 0)
		return node->GetValue();

	if (IsNodeTypeEqual(node, ExpressionNodeType::PLUS))
	{
		return Evaluate(dynamic_cast<PExpressionNode>(node->GetLeft())) +
			Evaluate(dynamic_cast<PExpressionNode>(node->GetRight()));
	}

	if (IsNodeTypeEqual(node, ExpressionNodeType::MINUS))
	{
		return Evaluate(dynamic_cast<PExpressionNode>(node->GetLeft())) -
			Evaluate(dynamic_cast<PExpressionNode>(node->GetRight()));
	}

	if (IsNodeTypeEqual(node, ExpressionNodeType::DIV))
	{
		// check division by zero ?
		return Evaluate(dynamic_cast<PExpressionNode>(node->GetLeft())) /
			Evaluate(dynamic_cast<PExpressionNode>(node->GetRight()));
	}

	if (IsNodeTypeEqual(node, ExpressionNodeType::MUL))
	{
		return Evaluate(dynamic_cast<PExpressionNode>(node->GetLeft())) *
			Evaluate(dynamic_cast<PExpressionNode>(node->GetRight()));
	}

	return 0.0f;
}

bool ASTInterpreter::IsNodeTypeEqual(PExpressionNode node, ExpressionNodeType nodeType)
{
	return node->GetType() == nodeType;
}

float ASTInterpreter::Interpret()
{
	if (root == nullptr) return 0.0f;
 
	if (root->GetNodeType() == ASTNodeType::Assignment)
	{
		string variableName = dynamic_cast<PVariableNode>(root->GetLeft())->GetName();
		if (hashTable.find(variableName) == hashTable.end())
			hashTable[variableName] = 0.0f;

		if (root->GetRight()->GetNodeType() == ASTNodeType::Expression)
			hashTable[variableName] = Evaluate(dynamic_cast<PExpressionNode>(root->GetRight()));

		return hashTable[variableName];
	}

	if (root->GetNodeType() == ASTNodeType::Expression)
	{
		return Evaluate(dynamic_cast<PExpressionNode>(root));
	}

	return 0.0f;
}
