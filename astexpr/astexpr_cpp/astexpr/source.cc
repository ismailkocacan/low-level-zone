#include <iostream>
#include <memory>
#include <map>
#include <string>
#include <Windows.h>
#include <typeinfo>

using namespace std;

enum ASTNodeType
{
	Assignment = 0,
	Variable = 1,
	Expression = 2
};

class ASTNode
{
private:
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


class AssignmentNode :
	public ASTNode
{
public:
	AssignmentNode();
	~AssignmentNode();
};

typedef AssignmentNode* PAssignmentNode;


class ASTInterpreter
{
private:
	PASTNode root;
	map<string, float> hashTable;
private:
	float Evaluate(PExpressionNode node);
	bool IsNodeTypeEqual(PExpressionNode node, ExpressionNodeType nodeType);
public:
	float Interpret();
public:
	ASTInterpreter(PASTNode node);
	~ASTInterpreter();
};

//-------------------------------------------------------------------//
ASTNode::ASTNode(){

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


AssignmentNode::AssignmentNode() :
	ASTNode(ASTNodeType::Assignment)
{

}


AssignmentNode::~AssignmentNode()
{
	cout << "AssignmentNode destructor" << endl;
}


ASTInterpreter::ASTInterpreter(PASTNode node) :
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


PASTNode CreateTestNodes()
{
	PExpressionNode exprNodePlus = new ExpressionNode(ExpressionNodeType::PLUS);
	PExpressionNode nodeValue_1 = new ExpressionNode(1);

	PExpressionNode nodeMul = new ExpressionNode(ExpressionNodeType::MUL);
	PExpressionNode nodeValue_2 = new ExpressionNode(2);
	PExpressionNode nodeValue_3 = new ExpressionNode(3);

	exprNodePlus->SetLeft(nodeValue_1);
	exprNodePlus->SetRight(nodeMul);

	nodeMul->SetLeft(nodeValue_2);
	nodeMul->SetRight(nodeValue_3);

	PAssignmentNode assignmentNode = new AssignmentNode();
	assignmentNode->SetLeft(new VariableNode("x"));
	assignmentNode->SetRight(exprNodePlus);

	return assignmentNode;
}

int main()
{
	DWORD start, finish;
	float result = 0;
	start = GetTickCount();

	PASTNode root = CreateTestNodes();
	shared_ptr<ASTInterpreter> interpreter(new ASTInterpreter(root));
	result = interpreter->Interpret();
	finish = GetTickCount();

	cout << "Result : " << result << endl;
	cout << "Time Diff (ms) : " << int(finish - start) << endl;

	return 0;
}