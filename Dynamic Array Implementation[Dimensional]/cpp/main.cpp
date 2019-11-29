
/*
  Author   : isocan
  Purpose  : How to implement dimensional dynamic array using pointers.
  DateTime : 17.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects
  Type variable[Col_Size][Row_Size]
  Element_Adress = Base_Adress + (Col_Index * Row_Size + Row_Index) * Element_Size
*/
#include <cstdint>
#include <string> 
#include <stdexcept>
#include <sstream>
#include <iostream>

typedef struct Index{
  int32_t colIndex;
  int32_t rowIndex;
} Index, *PIndex;

template <class Type>
class DimensionalArray{
private:
   const std::string IndexOutOfRangeException = "IndexOutOfRangeException at "; 
   Type* fBaseAdress;  
   int32_t fColCount;
   int32_t fRowCount;
   int32_t fMemorySize; 
private:
   std::string ToString(int32_t value){
      std::ostringstream ss;
      ss << value;
      return ss.str();
   }

   std::string GetMessage(std::string indexName,int32_t index){
      return std::string(IndexOutOfRangeException + indexName + "["+ ToString(index)+"]");
   }

   void RangeCheck(int32_t colIndex, int32_t rowIndex){
     if ((colIndex < 0) || (colIndex > fColCount - 1))
       throw std::out_of_range(GetMessage("colIndex",colIndex).c_str());
 
     if ((rowIndex < 0) || (rowIndex > fRowCount - 1))
       throw std::out_of_range(GetMessage("rowIndex",rowIndex).c_str());
   }

   int32_t Offset(int32_t colIndex, int32_t rowIndex){
     RangeCheck(colIndex,rowIndex);
 	   return (colIndex * fRowCount + rowIndex) * sizeof(Type);
   } 

   Type* CalculateElementAdress(int32_t colIndex, int32_t rowIndex){
     return fBaseAdress + Offset(colIndex,rowIndex);
   }

   void CalculateMemorySize(){
     fMemorySize =  sizeof(Type) * (fColCount * fRowCount);
   }
   
   void SetColAndRowCount(int32_t colCount,int32_t rowCount){
      fColCount = colCount;
      fRowCount = rowCount;
   }

   void MemoryAllocate(){
     CalculateMemorySize();
     fBaseAdress = (Type*)malloc(fMemorySize);
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

  Type & operator[](PIndex index){ 
     Type* elementAdress = CalculateElementAdress(index->colIndex, index->rowIndex);
     return *elementAdress;
  }

  void ReSize(int32_t colCount,int32_t rowCount){
     MemoryFree();
     SetColAndRowCount(colCount,rowCount);
     fMemorySize = CalculateMemorySize();
     fBaseAdress = (Type*)realloc(fBaseAdress,fMemorySize);
  }

  int32_t GetColCount() const{
    return fColCount;
  }

  int32_t GetRowCount() const{
    return fRowCount;
  }
};


int main(){    
    try{
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

        Index index = { 0, 0 };
        int32_t val = myTwoDimensionArray[&index];
        myTwoDimensionArray[&index] = 31;

        // test
        for (size_t colIndex = 0; colIndex < myTwoDimensionArray.GetColCount(); colIndex++){
          for (size_t rowIndex = 0; rowIndex < myTwoDimensionArray.GetRowCount(); rowIndex++){
              int32_t value = myTwoDimensionArray.GetElement(colIndex,rowIndex);
              std::cout << "["<< colIndex << "," << rowIndex <<"]" << value << std::endl;
          }      
        }
    }
    catch(const std::exception& e){
      std::cerr << e.what() << '\n';
    }
    
    system("pause");
    return EXIT_SUCCESS;
}