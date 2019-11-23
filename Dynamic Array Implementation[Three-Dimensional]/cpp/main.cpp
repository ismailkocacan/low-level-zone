/*
  Author   : isocan
  Purpose  : How to implement Three-dimensional dynamic array using pointers.
  DateTime : 21.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects

  Type variable[Depth_Size][Col_Size][Row_Size];
  Element_Adress = Base_Adress + ( (Depth_Index * Col_Size + Col_Index) * Row_Size + Row_Index) * Element_Size
*/
#include <cstdint>
#include <string> 
#include <stdexcept>
#include <sstream>
#include <iostream>

template <class Type>
class ThreeDimensionalArray{
private:
   const std::string IndexOutOfRangeException = "IndexOutOfRangeException at "; 
   Type* fBaseAdress;  
   int32_t fColCount;
   int32_t fRowCount;
   int32_t fDepthSize;
   int32_t fMemorySize;
private:
   std::string ToString(int32_t value){
      std::string outString;
      std::stringstream ss;
      ss << value;
      outString = ss.str();
      return outString;
   }

   std::string GetMessage(std::string indexName,int32_t index){
      return std::string(IndexOutOfRangeException + indexName + "["+ ToString(index)+"]");
   }

   void RangeCheck(int32_t depthIndex, int32_t colIndex, int32_t rowIndex){
    if ((depthIndex < 0) || (colIndex > fDepthSize - 1))
       throw std::out_of_range(GetMessage("depthIndex",depthIndex).c_str());
 
     if ((colIndex < 0) || (colIndex > fColCount - 1))
       throw std::out_of_range(GetMessage("colIndex",colIndex).c_str());
 
     if ((rowIndex < 0) || (rowIndex > fRowCount - 1))
       throw std::out_of_range(GetMessage("rowIndex",rowIndex).c_str());
   }

   int32_t Offset(int32_t depthIndex, int32_t colIndex, int32_t rowIndex){
     RangeCheck(depthIndex,colIndex,rowIndex); 	  
     return ( (depthIndex * fColCount + colIndex) * fRowCount + rowIndex) * sizeof(Type);
   } 

   Type* CalculateElementAdress(int32_t depthIndex, int32_t colIndex, int32_t rowIndex){
     return fBaseAdress + Offset(depthIndex, colIndex, rowIndex);
   }

   void CalculateMemorySize(){
     fMemorySize = sizeof(Type) * fDepthSize * (fColCount * fRowCount);
   }
   
   void SetDepthAndColAndRowCount(int32_t depthSize,int32_t colCount,int32_t rowCount){
      fDepthSize = depthSize;
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
  ThreeDimensionalArray(int32_t depthSize,int32_t colCount,int32_t rowCount):
    fDepthSize(depthSize),
    fColCount(colCount),
    fRowCount(rowCount){
    MemoryAllocate();
  }
  ~ThreeDimensionalArray() {
    MemoryFree();
  }      
public:
  Type GetElement(int32_t depthIndex,int32_t colIndex, int32_t rowIndex){
      Type* elementAdress = CalculateElementAdress(depthIndex,colIndex,rowIndex);
      return *elementAdress;
  }
  
  void SetElement(int32_t depthIndex,int32_t colIndex, int32_t rowIndex, Type value){
      Type* elementAdress = CalculateElementAdress(depthIndex,colIndex,rowIndex);
      *elementAdress = value;
  }

  void ReSize(int32_t depthSize, int32_t colCount,int32_t rowCount){
     MemoryFree();
     SetDepthAndColAndRowCount(depthSize,colCount,rowCount);
     CalculateMemorySize();
     fBaseAdress = (Type*)realloc(fBaseAdress,fMemorySize);
  }

  int32_t GetColCount(){
    return fColCount;
  }

  int32_t GetRowCount(){
    return fRowCount;
  }

  int32_t GetDepthSize(){
    return fDepthSize;
  }

  int32_t GetMemorySize(){
    return fMemorySize;
  }
};

int main(){    
    ThreeDimensionalArray<int32_t> anArray(2,2,2); // 2 adet, 2 sütun, 2 satırlık tablo

    // access elements ( set)
    for (size_t depthIndex = 0; depthIndex < anArray.GetDepthSize(); depthIndex++)
         for (size_t colIndex = 0; colIndex < anArray.GetColCount(); colIndex++)
             for (size_t rowIndex = 0; rowIndex < anArray.GetRowCount(); rowIndex++)
                     anArray.SetElement(depthIndex,colIndex,rowIndex, depthIndex + colIndex + rowIndex);
 
    // access elements ( get)
    for (size_t depthIndex = 0; depthIndex < anArray.GetDepthSize(); depthIndex++)
         for (size_t colIndex = 0; colIndex < anArray.GetColCount(); colIndex++)
             for (size_t rowIndex = 0; rowIndex < anArray.GetRowCount(); rowIndex++)
                   std::cout <<  anArray.GetElement(depthIndex,colIndex,rowIndex) << std::endl;
  
    system("pause");
    return EXIT_SUCCESS;
}