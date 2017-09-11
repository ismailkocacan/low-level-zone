package com.iso.astexpr;

public class Main
{
    public static void main(String[] args)
    {
        ExpressionNode exprNodePlus = ExpressionNode.Create(ExpressionNodeType.PLUS);
        ExpressionNode nodeValue_1 = ExpressionNode.Create(1);

        ExpressionNode nodeMul = ExpressionNode.Create(ExpressionNodeType.MUL);
        ExpressionNode nodeValue_2 = ExpressionNode.Create(2);
        ExpressionNode nodeValue_3 = ExpressionNode.Create(3);

        exprNodePlus.setLeft(nodeValue_1);
        exprNodePlus.setRight(nodeMul);

        nodeMul.setLeft(nodeValue_2);
        nodeMul.setRight(nodeValue_3);

        AssignmentNode assignmentNode = new AssignmentNode();
        assignmentNode.setLeft(new VariableNode("x"));
        assignmentNode.setRight(exprNodePlus);

        double result = 0.0;

        ASTInterpreter interpreter = new ASTInterpreter();
        interpreter.printExpressionNodes(exprNodePlus);

        result = interpreter.interpret(assignmentNode);
        result = interpreter.interpret(exprNodePlus);

        System.out.println();
        System.out.println(result);

    }
}
