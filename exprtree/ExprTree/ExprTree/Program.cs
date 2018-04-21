using System;
using System.Linq.Expressions;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

public class Calculate
{
    public int Calc(int x, int y)
    {
        return x * y;
    }
}

class Program
{
    static void Main(string[] args)
    {
        ParameterExpression variableExp = Expression.Variable(typeof(int), "X");
        ConstantExpression consExp = Expression.Constant(5, typeof(int));
        BinaryExpression binaryExp = Expression.Assign(variableExp, consExp);

        ParameterExpression variableExp2 = Expression.Variable(typeof(int), "Y");
        ConstantExpression consExp2 = Expression.Constant(6, typeof(int));
        BinaryExpression binaryExp2 = Expression.Assign(variableExp2, consExp2);

        BinaryExpression exp3 = Expression.Add(variableExp, variableExp2);
        NewExpression newExpr = Expression.New(typeof(Calculate));
       
        MethodCallExpression methodCallExpr = Expression.Call(
            newExpr, 
            typeof(Calculate).GetMethod("Calc"), 
            new Expression[] { variableExp, variableExp2 }
            );


        BlockExpression blockExp = Expression.Block(
            new ParameterExpression[] { variableExp, variableExp2 },
            binaryExp, binaryExp2, exp3, newExpr, methodCallExpr
        );

        Console.WriteLine(blockExp.ToString());
        int result = Expression.Lambda<Func<int>>(blockExp).Compile().Invoke();
    }
}
