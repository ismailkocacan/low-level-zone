/*
   1 + 2 * 3 

	'+'
   /   \
 '1'    *
      /   \ 
    '2'   '3'
*/

using System;

namespace astexpr
{
    enum ASTNodeType
    {
        UNDEFINED = 0,
        PLUS = 1,
        MINUS = 2,
        DIV = 3,
        MUL = 4
    };

    class ASTNode
    {
        public int Value { get; set; }
        public ASTNodeType Type { get; set; }
        public ASTNode Left { get; set; }
        public ASTNode Right { get; set; }

        void SetChildDefaultValues()
        {
            Left = null;
            Right = null;
        }

        public ASTNode()
        {
            Type = ASTNodeType.UNDEFINED;
            Value = 0;
            SetChildDefaultValues();
        }


        public ASTNode(int value)
        {
            Type = ASTNodeType.UNDEFINED;
            Value = value;
            SetChildDefaultValues();
        }

        public ASTNode(ASTNodeType nodeType)
        {
            Type = nodeType;
            Value = 0;
            SetChildDefaultValues();
        }
      
        public static ASTNode Create(int value)
        {
            return new ASTNode(value);
        }

        public static ASTNode Create(ASTNodeType nodeType)
        {
            return new ASTNode(nodeType);
        }
    }

    class Program
    {
        static float Evaluate(ASTNode node)
        {
            if (node.Value != 0)
            {
                return node.Value;
            }

            if (node.Type == ASTNodeType.PLUS)
            {
                return Evaluate(node.Left) + Evaluate(node.Right);
            }

            if (node.Type == ASTNodeType.MINUS)
            {
                return Evaluate(node.Left) - Evaluate(node.Right);
            }

            if (node.Type == ASTNodeType.DIV)
            {
                // check division by zero ?
                return Evaluate(node.Left) / Evaluate(node.Right);
            }

            if (node.Type == ASTNodeType.MUL)
            {
                return Evaluate(node.Left) * Evaluate(node.Right);
            }

            return 0.0f;
        }

 
        static void PrintNodes(ASTNode node)
        {
            if (node == null) return;

            if (node.Type != ASTNodeType.UNDEFINED)
            {
                Console.WriteLine(node.Type.ToString());
                Console.WriteLine(@"/  \");
            }
            else
            {
                Console.Write(node.Value.ToString() + " ");
            }

            PrintNodes(node.Left);
            PrintNodes(node.Right);
        }


        static void Main(string[] args)
        {
            ASTNode nodePlus = ASTNode.Create(ASTNodeType.PLUS);
            ASTNode nodeValue_1 = ASTNode.Create(1);

            ASTNode nodeMul = ASTNode.Create(ASTNodeType.MUL);
            ASTNode nodeValue_2 = ASTNode.Create(2);
            ASTNode nodeValue_3 = ASTNode.Create(3);
           
            nodePlus.Left = nodeValue_1;
            nodePlus.Right = nodeMul;

            nodeMul.Left = nodeValue_2;
            nodeMul.Right = nodeValue_3;

            PrintNodes(nodePlus);

            float result = 0.0f;
            result = Evaluate(nodePlus);

            Console.WriteLine(Environment.NewLine);
            Console.WriteLine(result);
           
        }
    }
}
