#include "assignmentnode.h"


AssignmentNode::AssignmentNode() :
	ASTNode(ASTNodeType::Assignment)
{

}


AssignmentNode::~AssignmentNode()
{
	cout << "AssignmentNode destructor" << endl;
}
