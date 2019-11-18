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
    private IntPtr baseAdress;

    public DimensionalArray(int colCount, int rowCount)
    {
        this.colCount = colCount;
        this.rowCount = rowCount;
        baseAdress = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(T)) * (colCount * rowCount));
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

    public int ColCount { get { return colCount; } }
    public int RowCount { get { return rowCount; } }

    public T this[int colIndex, int rowIndex]
    {
        get
        {
            void* elementAdress = CalculateElementAdress(colIndex, rowIndex).ToPointer();
            T* tp = (T*)elementAdress;
            return *tp;
        }
        set
        {
            void* elementAdress = CalculateElementAdress(colIndex, rowIndex).ToPointer();
            T* tp = (T*)elementAdress;
            *tp = value;
        }
    }

    ~DimensionalArray()
    {
        Marshal.FreeHGlobal(baseAdress);
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
