#ifndef ASSIGNMENTNODE_H
#define ASSIGNMENTNODE_H

#include "astnode.h"
#include <iostream>

using namespace std;


class AssignmentNode :
	public ASTNode
{
public:
	AssignmentNode();
    ~AssignmentNode();
};

typedef AssignmentNode* PAssignmentNode;
#endif

