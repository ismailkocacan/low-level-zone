
/*
  Author   : isocan
  Purpose  : How to implement dimensional dynamic array using pointers.
  DateTime : 17.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects
  Type variable[Col_Size][Row_Size]
  Element_Adress = Base_Adress + (Col_Index * Row_Size + Row_Index) * Element_Size
*/

#include <iostream>

template <class Type>
class DimensionalArray{
private:
   const std::string IndexOutOfRangeException = "IndexOutOfRangeException at "; 
   Type* fBaseAdress;  
   int32_t fColCount;
   int32_t fRowCount;
private:
   int32_t Offset(int32_t colIndex, int32_t rowIndex){
 	   return (colIndex * fRowCount + rowIndex) * sizeof(Type);
   } 

   Type* CalculateElementAdress(int32_t colIndex, int32_t rowIndex){
     return fBaseAdress + Offset(colIndex,rowIndex);
   }

   size_t CalculateMemorySize(int32_t colCount,int32_t rowCount){
     return sizeof(Type) * (colCount * rowCount);
   }
   
   void SetColAndRowCount(int32_t colCount,int32_t rowCount){
      fColCount = colCount;
      fRowCount = rowCount;
   }

   void MemoryAllocate(){
     fBaseAdress = (Type*)malloc(CalculateMemorySize(fColCount,fRowCount));
   }

   void MemoryFree(){
     free(fBaseAdress);   
   }
public:
  DimensionalArray(int32_t colCount,int32_t rowCount):
    fColCount(colCount),
    fRowCount(rowCount){
    MemoryAllocate();
  }
  ~DimensionalArray() {
    MemoryFree();
  }      
public:
  Type GetElement(int32_t colIndex, int32_t rowIndex){
      Type* elementAdress = CalculateElementAdress(colIndex,rowIndex);
      return *elementAdress;
  }
  
  void SetElement(int32_t colIndex, int32_t rowIndex, Type value){
      Type* elementAdress = CalculateElementAdress(colIndex,rowIndex);
      *elementAdress = value;
  }

  void ReSize(int32_t colCount,int32_t rowCount){
     MemoryFree();
     SetColAndRowCount(colCount,rowCount);
     fBaseAdress = (Type*)realloc(fBaseAdress,CalculateMemorySize(colCount,rowCount));
  }

  int32_t GetColCount(){
    return fColCount;
  }

  int32_t GetRowCount(){
    return fRowCount;
  }
};


int main(){    
    DimensionalArray<int32_t> myTwoDimensionArray(2,2); // 2 sütun, 2 satır
    myTwoDimensionArray.SetElement(0,0,31); // 1. kolon, 1. satır.
    myTwoDimensionArray.SetElement(1,0,32); // 2. kolon, 1. satır

    myTwoDimensionArray.SetElement(0,1,33); // 1. kolon, 2. satır
    myTwoDimensionArray.SetElement(1,1,34); // 2. kolon, 2. satır
    
    /*
    int32_t value_0_0 = myTwoDimensionArray.GetElement(0,0); 
    std::cout << "1. kolon, 1. satır Değeri: " << value_0_0 << std::endl;
    int32_t value_1_0 = myTwoDimensionArray.GetElement(1,0); 
    std::cout << "2. kolon, 1. satır Değeri: " << value_1_0 << std::endl;

    int32_t value_0_1 = myTwoDimensionArray.GetElement(0,1); 
    std::cout << "1. kolon, 2. satır Değeri: " << value_0_1 << std::endl;
    int32_t value_1_1 = myTwoDimensionArray.GetElement(1,1);  
    std::cout << "2. kolon, 2. satır Değeri: " << value_1_1 << std::endl;
   */

    // test
    for (size_t colIndex = 0; colIndex < myTwoDimensionArray.GetColCount(); colIndex++){
      for (size_t rowIndex = 0; rowIndex < myTwoDimensionArray.GetRowCount(); rowIndex++){
          int32_t value = myTwoDimensionArray.GetElement(colIndex,rowIndex);
          std::cout << "["<< colIndex << "," << rowIndex <<"]" << value << std::endl;
      }      
    }
   
    system("pause");
    return EXIT_SUCCESS;
}