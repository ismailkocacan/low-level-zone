package com.iso.astexpr;

public class VariableNode extends ASTNode
{
    private String name;

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public VariableNode(String name)
    {
        super(ASTNodeType.Variable);
        setName(name);
    }
}
