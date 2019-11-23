/*
  Author   : isocan
  Purpose  : How to implement dimensional dynamic array using pointers.
  DateTime : 18.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects
  Type variable[Col_Size][Row_Size]
  Element_Adress = Base_Adress + (Col_Index * Row_Size + Row_Index) * Element_Size

  https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-7.3/blittable (Unmanaged type constraint)
*/


using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

unsafe class DimensionalArray<T> where T : unmanaged
{
    private int colCount;
    private int rowCount;
    private int memorySize;
    private IntPtr baseAdress;

    public DimensionalArray(int colCount, int rowCount)
    {
        SetColAndRowCount(colCount, rowCount);
        MemoryAllocate();
    }

    private int Offset(int colIndex, int rowIndex)
    {
        return (colIndex * rowCount + rowIndex) * Marshal.SizeOf(typeof(T));
    }

    private IntPtr CalculateElementAdress(int colIndex, int rowIndex)
    {
        IntPtr elementAdress = IntPtr.Add(baseAdress, Offset(colIndex, rowIndex));
        return elementAdress;
    }

    private void CalculateMemorySize()
    {
        memorySize = Marshal.SizeOf(typeof(T)) * (colCount * rowCount);
    }

    private void SetColAndRowCount(int colCount, int rowCount)
    {
        this.colCount = colCount;
        this.rowCount = rowCount;
    }

    private void MemoryAllocate()
    {
        CalculateMemorySize();
        baseAdress = Marshal.AllocHGlobal(memorySize);
    }

    private void ReSize(int colCount, int rowCount)
    {
        /*
         cb:IntPtr
         The new size of the allocated block. 
         This is not a pointer; it is the byte count you are requesting, cast to type IntPtr. 
         If you pass a pointer, it is treated as a size.
         https://docs.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.marshal.reallochglobal?view=netframework-4.8
         */
        MemoryFree();
        SetColAndRowCount(colCount, rowCount);
        CalculateMemorySize();
        baseAdress = Marshal.ReAllocHGlobal(baseAdress, (IntPtr)memorySize);
    }

    private void MemoryFree()
    {
        Marshal.FreeHGlobal(baseAdress);
    }

    public int ColCount { get { return colCount; } }
    public int RowCount { get { return rowCount; } }

    private T* GetElementAdress(int colIndex, int rowIndex)
    {
        void* unTypedElementAdress = CalculateElementAdress(colIndex, rowIndex).ToPointer();
        T* typedElementAdress = (T*)unTypedElementAdress;
        return typedElementAdress;
    }

    public T this[int colIndex, int rowIndex]
    {
        get
        {
            return *GetElementAdress(colIndex, rowIndex);
        }
        set
        {
            *GetElementAdress(colIndex, rowIndex) = value;
        }
    }

    ~DimensionalArray()
    {
        MemoryFree();
    }
}

class Program
{
    static void Main(string[] args)
    {
        DimensionalArray<int> myTwoDimensionArray = new DimensionalArray<int>(2, 2);
        myTwoDimensionArray[0, 0] = 31;
        myTwoDimensionArray[1, 0] = 32;
        myTwoDimensionArray[0, 1] = 33;
        myTwoDimensionArray[1, 1] = 34;

        // test
        for (int colIndex = 0; colIndex < myTwoDimensionArray.ColCount; colIndex++)
        {
            for (int rowIndex = 0; rowIndex < myTwoDimensionArray.RowCount; rowIndex++)
            {
                int value = myTwoDimensionArray[colIndex, rowIndex];
                MessageBox.Show(value.ToString());
            }
        }
    }
}