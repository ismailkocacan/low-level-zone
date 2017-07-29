#include <stdio.h>
#include <iostream>
#include <typeinfo>
#include "ast.h"

using namespace std;

PASTNode CreateTestNodes()
{
	PASTNode nodePlus = new ASTNode();
	nodePlus->SetType(ASTNodeType::PLUS);

	PASTNode nodeValue_1 = new ASTNode();
	nodeValue_1->SetValue(1);


	PASTNode nodeMul = new ASTNode();
	nodeMul->SetType(ASTNodeType::MUL);

	PASTNode nodeValue_2 = new ASTNode();
	nodeValue_2->SetValue(2);

	PASTNode nodeValue_3 = new ASTNode();
	nodeValue_3->SetValue(3);


	nodePlus->SetLeft(nodeValue_1);
	nodePlus->SetRight(nodeMul);

	nodeMul->SetLeft(nodeValue_2);
	nodeMul->SetRight(nodeValue_3);
	return nodePlus;
}

int main()
{
	float result = Evualate(CreateTestNodes());
	cout << "Sonuç : " << result;

	return 0;
}