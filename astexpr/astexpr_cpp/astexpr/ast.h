#include <stdio.h>

enum ASTNodeType
{
	UNDEFINED = 0,
	PLUS = 1,
	MINUS = 2,
	DIV = 3,
	MUL = 4
};

class ASTNode
{
private:
	float value;
	ASTNodeType type;
	ASTNode* left;
	ASTNode* right;
public:
	float GetValue();
	ASTNodeType GetType();
	ASTNode* GetLeft();
	ASTNode* GetRight();
	
	void SetValue(float value);
	void SetType(ASTNodeType type);
	void SetLeft(ASTNode* left);
	void SetRight(ASTNode* right);
public:
	ASTNode();
	~ASTNode();
};

typedef ASTNode* PASTNode;



typedef struct ASTNode2
{
	float Value = 0;
	ASTNodeType Type;
	ASTNode2* Left;
	ASTNode2* Right;
} *PASTNode2;


bool IsNodeTypeEqual(PASTNode node, ASTNodeType nodeType);
bool IsNodeTypeEqual(PASTNode2 node, ASTNodeType nodeType);

float Evaluate(PASTNode node);
float Evaluate(PASTNode2 node);