package com.iso.astexpr;


public class ExpressionNode extends ASTNode
{
    private int value;
    private  ExpressionNodeType type;

    public int getValue()
    {
        return value;
    }

    public void setValue(int value)
    {
        this.value = value;
    }

    public ExpressionNodeType getType()
    {
        return type;
    }

    public void setType(ExpressionNodeType type)
    {
        this.type = type;
    }

    private void SetChildDefaultValues()
    {
        setLeft(null);
        setRight(null);
    }

    public ExpressionNode()
    {
        super(ASTNodeType.Expression);
        setType(ExpressionNodeType.UNDEFINED);
        setValue(0);
    }

    public ExpressionNode(int value)
    {
        super(ASTNodeType.Expression);
        setValue(value);
        setType(ExpressionNodeType.UNDEFINED);
        SetChildDefaultValues();
    }

    public ExpressionNode(ExpressionNodeType nodeType)
    {
        super(ASTNodeType.Expression);
        setValue(0);
        setType(nodeType);
        SetChildDefaultValues();
    }

    public static ExpressionNode Create(int value)
    {
        return new ExpressionNode(value);
    }

    public static ExpressionNode Create(ExpressionNodeType nodeType)
    {
        return new ExpressionNode(nodeType);
    }
}