package com.iso.astexpr;


public class ASTNode
{
    private ASTNode left;
    private ASTNode right;
    private ASTNodeType nodeType;

    public ASTNode getLeft()
    {
        return left;
    }

    public void setLeft(ASTNode left)
    {
        this.left = left;
    }

    public ASTNode getRight()
    {
        return right;
    }

    public void setRight(ASTNode right)
    {
        this.right = right;
    }

    public ASTNodeType getNodeType()
    {
        return nodeType;
    }

    public void setNodeType(ASTNodeType nodeType)
    {
        this.nodeType = nodeType;
    }

    public ASTNode(ASTNodeType type)
    {
        setNodeType(type);
    }
}
