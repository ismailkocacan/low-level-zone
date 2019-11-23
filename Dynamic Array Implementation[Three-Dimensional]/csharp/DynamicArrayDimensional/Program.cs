/*
  Author   : isocan
  Purpose  : How to implement Three-dimensional dynamic array using pointers.
  DateTime : 21.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects

  Type variable[Depth_Size][Col_Size][Row_Size];
  Element_Adress = Base_Adress + ( (Depth_Index * Col_Size + Col_Index) * Row_Size + Row_Index) * Element_Size

  https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-7.3/blittable (Unmanaged type constraint)
*/


using System;
using System.Windows.Forms;
using System.Runtime.InteropServices;

unsafe class ThreeDimensionalArray<T> where T : unmanaged
{
    private const string IndexOutOfRangeException = "IndexOutOfRangeException at %s %d";

    private int depthSize;
    private int colCount;
    private int rowCount;
    private int memorySize;
    private IntPtr baseAdress;

    public ThreeDimensionalArray(int depthSize, int colCount, int rowCount)
    {
        SetDepthAndColAndRowCount(depthSize, colCount, rowCount);
        MemoryAllocate();
    }

    private string GetMessage(string indexName, int index)
    {
        return string.Format(IndexOutOfRangeException, indexName, index);
    }

    private void RangeCheck(int depthIndex, int colIndex, int rowIndex)
    {
        if ((depthIndex < 0) || (colIndex > depthSize - 1))
            throw new Exception(GetMessage("depthIndex", depthIndex));

        if ((colIndex < 0) || (colIndex > colCount - 1))
            throw new Exception(GetMessage("colIndex", colIndex));

        if ((rowIndex < 0) || (rowIndex > rowCount - 1))
            throw new Exception(GetMessage("rowIndex", rowIndex));
    }

    private int Offset(int depthIndex, int colIndex, int rowIndex)
    {
        RangeCheck(depthIndex, colIndex, rowIndex);
        return ((depthIndex * colCount + colIndex) * rowCount + rowIndex) * Marshal.SizeOf(typeof(T));
    }

    private IntPtr CalculateElementAdress(int depthIndex, int colIndex, int rowIndex)
    {
        IntPtr elementAdress = IntPtr.Add(baseAdress, Offset(depthIndex, colIndex, rowIndex));
        return elementAdress;
    }

    private void CalculateMemorySize()
    {
        memorySize = Marshal.SizeOf(typeof(T)) * this.depthSize * (this.colCount * this.rowCount);
    }

    private void SetDepthAndColAndRowCount(int depthSize, int colCount, int rowCount)
    {
        this.depthSize = depthSize;
        this.colCount = colCount;
        this.rowCount = rowCount;
    }

    private void MemoryAllocate()
    {
        CalculateMemorySize();
        baseAdress = Marshal.AllocHGlobal(memorySize);
    }

    private void ReSize(int depthSize, int colCount, int rowCount)
    {
        /*
         cb:IntPtr
         The new size of the allocated block. 
         This is not a pointer; it is the byte count you are requesting, cast to type IntPtr. 
         If you pass a pointer, it is treated as a size.
         https://docs.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.marshal.reallochglobal?view=netframework-4.8
         */
        MemoryFree();
        SetDepthAndColAndRowCount(depthSize, colCount, rowCount);
        CalculateMemorySize();
        baseAdress = Marshal.ReAllocHGlobal(baseAdress, (IntPtr)memorySize);
    }

    private void MemoryFree()
    {
        Marshal.FreeHGlobal(baseAdress);
    }

    public int DepthSize { get { return depthSize; } }
    public int ColCount { get { return colCount; } }
    public int RowCount { get { return rowCount; } }
    public int MemorySize { get { return memorySize; } }

    private T* GetElementAdress(int depthIndex, int colIndex, int rowIndex)
    {
        void* unTypedElementAdress = CalculateElementAdress(depthIndex, colIndex, rowIndex).ToPointer();
        T* typedElementAdress = (T*)unTypedElementAdress;
        return typedElementAdress;
    }

    public T this[int depthIndex, int colIndex, int rowIndex]
    {
        get
        {
            return *GetElementAdress(depthIndex, colIndex, rowIndex);
        }
        set
        {
            *GetElementAdress(depthIndex, colIndex, rowIndex) = value;
        }
    }

    ~ThreeDimensionalArray()
    {
        MemoryFree();
    }
}

class Program
{
    static void Main(string[] args)
    {
        ThreeDimensionalArray<int> anArray = new ThreeDimensionalArray<int>(2, 2, 2);
        //anArray.MemorySize;

        // set values
        for (int depthIndex = 0; depthIndex < anArray.DepthSize; depthIndex++)
            for (int colIndex = 0; colIndex < anArray.ColCount; colIndex++)
                for (int rowIndex = 0; rowIndex < anArray.RowCount; rowIndex++)
                    anArray[depthIndex, colIndex, rowIndex] = depthIndex + colIndex + rowIndex;

        // access elements
        for (int depthIndex = 0; depthIndex < anArray.DepthSize; depthIndex++)
        {
            for (int colIndex = 0; colIndex < anArray.ColCount; colIndex++)
            {
                for (int rowIndex = 0; rowIndex < anArray.RowCount; rowIndex++)
                {
                    int value = anArray[depthIndex, colIndex, rowIndex];
                }
            }
        }
    }
}
