
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
public:
  DimensionalArray(int32_t colCount,int32_t rowCount):
    fColCount(colCount),
    fRowCount(rowCount){
    fBaseAdress = (Type*)malloc(sizeof(Type) * rowCount);
  }
  ~DimensionalArray() {
    free(fBaseAdress);   
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
};


int main(){    
    DimensionalArray<int32_t> myTwoDimensionArray(2,2); // 2 sütun, 2 satır
    myTwoDimensionArray.SetElement(0,0,31); // 1. kolon, 1. satır.
    myTwoDimensionArray.SetElement(1,0,32); // 2. kolon, 1. satır

    myTwoDimensionArray.SetElement(0,1,33); // 1. kolon, 2. satır
    myTwoDimensionArray.SetElement(1,1,34); // 2. kolon, 2. satır
    
    int32_t value;
    value = myTwoDimensionArray.GetElement(0,0); 
    std::cout << "1. kolon, 1. satır Değeri: " << value << std::endl;
    value = myTwoDimensionArray.GetElement(1,0); 
    std::cout << "2. kolon, 1. satır Değeri: " << value << std::endl;

    value = myTwoDimensionArray.GetElement(0,1); 
    std::cout << "1. kolon, 2. satır Değeri: " << value << std::endl;
    value = myTwoDimensionArray.GetElement(1,1);  
    std::cout << "2. kolon, 2. satır Değeri: " << value << std::endl;

    system("pause");
    return EXIT_SUCCESS;
}