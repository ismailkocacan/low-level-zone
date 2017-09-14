#include <stdio.h>
#include <iostream>
#include <memory>
#include <Windows.h>
#include <typeinfo>
#include "astnode.h"
#include "expressionnode.h"
#include "astinterpreter.h"

using namespace std;

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