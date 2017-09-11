package com.iso.astexpr;

public class ASTInterpreter
{
    private SymbolTable symbolTable;

    public ASTInterpreter()
    {
        symbolTable = new SymbolTable();
    }

    private double evaluate(ExpressionNode node)
    {
        if (node.getValue() != 0)
            return node.getValue();

        if (node.getType() == ExpressionNodeType.PLUS)
        {
            return evaluate((ExpressionNode)node.getLeft()) + evaluate((ExpressionNode)node.getRight());
        }

        if (node.getType() == ExpressionNodeType.MINUS)
        {
            return evaluate((ExpressionNode)node.getLeft()) - evaluate((ExpressionNode)node.getRight());
        }

        if (node.getType() == ExpressionNodeType.DIV)
        {
            return evaluate((ExpressionNode)node.getLeft()) / evaluate((ExpressionNode)node.getRight());
        }

        if (node.getType() == ExpressionNodeType.MUL)
        {
            return evaluate((ExpressionNode)node.getLeft()) * evaluate((ExpressionNode)node.getRight());
        }

        return 0.0;
    }

    public void printExpressionNodes(ExpressionNode node)
    {
        if (node == null) return;

        if (node.getType() != ExpressionNodeType.UNDEFINED)
        {
            System.out.println(node.getType().toString());
            System.out.println("/  \\");
        }
        else
        {
            System.out.println(Integer.toString(node.getValue()) + " ");
        }

        printExpressionNodes((ExpressionNode)node.getLeft());
        printExpressionNodes((ExpressionNode)node.getRight());
    }

    public double interpret(ASTNode node)
    {
        if (node == null) return 0.0;

        if (node.getNodeType() == ASTNodeType.Assignment)
        {
            String variableName = ((VariableNode)node.getLeft()).getName();
            if (!symbolTable.containsKey(variableName)) symbolTable.put(variableName, 0);

            if (node.getRight().getNodeType() == ASTNodeType.Expression)
                symbolTable.put(variableName,evaluate((ExpressionNode)node.getRight()));

            return (double)symbolTable.get(variableName);
        }

        if (node.getNodeType() == ASTNodeType.Expression)
        {
            return evaluate((ExpressionNode)node);
        }

        return 0.0;
    }

}
