/*
   1 + 2 * 3  

	'+'
   /   \
 '1'    *
      /   \ 
    '2'   '3'

-----------------------------------
1 + 2 * 3      => expression
x              => variable 

x = 1 + 2 * 3  => assignment statement


  assignment node 
   /            \
 variable       expression
  x                	'+'
                   /   \
                 '1'    *
                      /   \ 
                    '2'   '3'    
-----------------------------------

*/

using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace astexpr
{

    class HashTable : Dictionary<string, object>
    {

    }

    [Serializable]
    enum ASTNodeType
    {
        Assignment = 0,
        Variable = 1,
        Expression = 2
    }



    [Serializable]
    class ASTNode
    {
        public ASTNode Left { get; set; }
        public ASTNode Right { get; set; }

        public ASTNodeType NodeType { get; set; }

        public ASTNode(ASTNodeType type)
        {
            this.NodeType = type;
            Left = null;
            Right = null;
        }
    }

    [Serializable]
    enum ExpressionNodeType
    {
        UNDEFINED = 0,
        PLUS = 1,
        MINUS = 2,
        DIV = 3,
        MUL = 4
    };

    [Serializable]
    class ExpressionNode : ASTNode
    {
        public int Value { get; set; }
        public ExpressionNodeType Type { get; set; }


        public new ExpressionNode Left { get; set; }
        public new ExpressionNode Right { get; set; }

        void SetChildDefaultValues()
        {
            Left = null;
            Right = null;
        }

        public ExpressionNode() :
            base(ASTNodeType.Expression)
        {
            Type = ExpressionNodeType.UNDEFINED;
            Value = 0;
            SetChildDefaultValues();
        }


        public ExpressionNode(int value) :
            base(ASTNodeType.Expression)
        {
            Type = ExpressionNodeType.UNDEFINED;
            Value = value;
            SetChildDefaultValues();
        }

        public ExpressionNode(ExpressionNodeType nodeType) :
            base(ASTNodeType.Expression)
        {
            Type = nodeType;
            Value = 0;
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

    [Serializable]
    class AssignmentNode : ASTNode
    {
        public AssignmentNode() :
            base(ASTNodeType.Assignment)
        {

        }
    }

    [Serializable]
    class VariableNode : ASTNode
    {
        public string Name { get; set; }

        public VariableNode(string name) :
            base(ASTNodeType.Variable)
        {
            this.Name = name;
        }
    }


    class ASTInterpreter
    {
        HashTable hashTable = new HashTable();

        public ASTInterpreter()
        {

        }

        public float Evaluate(ExpressionNode node)
        {
            if (node.Value != 0)
            {
                return node.Value;
            }

            if (node.Type == ExpressionNodeType.PLUS)
            {
                return Evaluate(node.Left) + Evaluate(node.Right);
            }

            if (node.Type == ExpressionNodeType.MINUS)
            {
                return Evaluate(node.Left) - Evaluate(node.Right);
            }

            if (node.Type == ExpressionNodeType.DIV)
            {
                // check division by zero ?
                return Evaluate(node.Left) / Evaluate(node.Right);
            }

            if (node.Type == ExpressionNodeType.MUL)
            {
                return Evaluate(node.Left) * Evaluate(node.Right);
            }

            return 0.0f;
        }

        public void PrintExpressionNodes(ExpressionNode node)
        {
            if (node == null) return;

            if (node.Type != ExpressionNodeType.UNDEFINED)
            {
                Console.WriteLine(node.Type.ToString());
                Console.WriteLine(@"/  \");
            }
            else
            {
                Console.Write(node.Value.ToString() + " ");
            }

            PrintExpressionNodes(node.Left);
            PrintExpressionNodes(node.Right);
        }

        public float Interpret(ASTNode node)
        {
            if (node == null) return 0.0f;

            if (node.NodeType == ASTNodeType.Assignment)
            {

                string variableName = ((VariableNode)node.Left).Name;
                if (!hashTable.ContainsKey(variableName)) hashTable.Add(variableName, 0);

                if (node.Right.NodeType == ASTNodeType.Expression)
                    hashTable[variableName] = Evaluate((ExpressionNode)node.Right);

                return (float)hashTable[variableName];
            }

            if (node.NodeType == ASTNodeType.Expression)
            {
                return Evaluate(node as ExpressionNode);
            }

            return 0.0f;
        }
    }


    interface ISerialization
    {
        void SaveToFile<T>(string filePath, T rootObject);
        T ReadFromFile<T>(string filePath);
    }

    public class ASTBinarySerialization : ISerialization
    {
        public T ReadFromFile<T>(string filePath)
        {
            T result;
            using (Stream stream = File.Open(filePath, FileMode.Open))
            {
                IFormatter formatter = new BinaryFormatter();
                result = (T)formatter.Deserialize(stream);
                stream.Close();
            }
            return result;
        }

        public void SaveToFile<T>(string filePath, T rootObject)
        {
            using (Stream stream = File.Open(filePath, FileMode.Create))
            {
                IFormatter formatter = new BinaryFormatter();
                formatter.Serialize(stream, rootObject);
                stream.Close();
            }
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            ExpressionNode exprNodePlus = ExpressionNode.Create(ExpressionNodeType.PLUS);
            ExpressionNode nodeValue_1 = ExpressionNode.Create(1);

            ExpressionNode nodeMul = ExpressionNode.Create(ExpressionNodeType.MUL);
            ExpressionNode nodeValue_2 = ExpressionNode.Create(2);
            ExpressionNode nodeValue_3 = ExpressionNode.Create(3);

            exprNodePlus.Left = nodeValue_1;
            exprNodePlus.Right = nodeMul;

            nodeMul.Left = nodeValue_2;
            nodeMul.Right = nodeValue_3;

            AssignmentNode assignmentNode = new AssignmentNode();
            assignmentNode.Left = new VariableNode("x");
            assignmentNode.Right = exprNodePlus;
            
            float result = 0.0f;
            ASTInterpreter interpreter = new ASTInterpreter();
            interpreter.PrintExpressionNodes(exprNodePlus);

            result = interpreter.Interpret(assignmentNode);
            result = interpreter.Interpret(exprNodePlus);

            Console.WriteLine(Environment.NewLine);
            Console.WriteLine(result);


            ISerialization serialization = new ASTBinarySerialization();
            serialization.SaveToFile<ASTNode>("ast.bin", exprNodePlus);
            ExpressionNode rootNode = serialization.ReadFromFile<ExpressionNode>("ast.bin");
            float result2 = 0.0f;
            ASTInterpreter interpreter2 = new ASTInterpreter();
            interpreter2.PrintExpressionNodes(rootNode);
            result2 = interpreter.Interpret(rootNode);
        }
    }
}
