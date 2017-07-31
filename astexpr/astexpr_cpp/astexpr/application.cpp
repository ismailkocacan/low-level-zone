#include <stdio.h>
#include <iostream>
#include <typeinfo>
#include <Windows.h>
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
	DWORD start, finish;
	float result = 0;

	start = GetTickCount();
	result = Evaluate(CreateTestNodes());
	finish = GetTickCount();
	cout << "Result : " << result << endl;
	cout << "Time Diff Heap (ms) : " << int(finish - start) << endl;



	start = GetTickCount();
	ASTNode2 A2NodePlus;
	A2NodePlus.Type = ASTNodeType::PLUS;

	ASTNode2 A2NodeValue_1;
	A2NodeValue_1.Value = 1;

	ASTNode2 A2NodeMul;
	A2NodeMul.Type = ASTNodeType::MUL;

	ASTNode2 A2NodeValue_2;
	ASTNode2 A2NodeValue_3;
	A2NodeValue_2.Value = 2;
	A2NodeValue_3.Value = 3;

	A2NodeMul.Left = &A2NodeValue_2;
	A2NodeMul.Right = &A2NodeValue_3;

	A2NodePlus.Left = &A2NodeValue_1;
	A2NodePlus.Right = &A2NodeMul;

	result = Evaluate(&A2NodePlus);
	finish = GetTickCount();
	cout << "Result : " << result << endl;
	cout << "Time Diff Stack (ms): " << int(finish - start) << endl;


	return 0;
}