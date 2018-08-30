using System;
using System.Runtime.InteropServices;

namespace DynamicArray
{

    struct S
    {
        int X;
        int Y;
    }

    class DynamicArray<T>
    {
        private int length;
        private int size;
        private IntPtr memBlock;

        public DynamicArray(int length)
        {
            this.length = length;
            int sizet = Marshal.SizeOf(typeof(T));
            this.size = length * sizet;
            memBlock = Marshal.AllocHGlobal(10);
        }

        private int Offset(int index)
        {
            return Marshal.SizeOf(typeof(T)) * index;
        }
        

        public T this[int index]
        {
            get
            {
                IntPtr pointer = IntPtr.Add(memBlock, Offset(index));
                return default(T);
            }
            set
            {
                IntPtr pointer = IntPtr.Add(memBlock, Offset(index));
            }
        }

        ~DynamicArray()
        {
            Marshal.FreeHGlobal(memBlock);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            DynamicArray<S> myArray0 = new DynamicArray<S>(5);
   
            DynamicArray<decimal> myArray1 = new DynamicArray<decimal>(5);

            DynamicArray<int> myArray2 = new DynamicArray<int>(5);
        }
    }
}
