/*
  Author   : isocan
  Purpose  : How to implement Three-dimensional dynamic array using pointers.
  DateTime : 21.11.2019

  Write Great Code: Volume 1: Understanding the Machine
  Composite Data Types and Memory Objects

  Type variable[Depth_Size][Col_Size][Row_Size];
  Element_Adress = Base_Adress + ( (Depth_Index * Col_Size + Col_Index) * Row_Size + Row_Index) * Element_Size
*/

#ifndef THREE_DIMENSIONAL_ARRAY
#define THREE_DIMENSIONAL_ARRAY

#include <cstdint>
#include <string> 
#include <stdexcept>
#include <sstream>
#include <iostream>

typedef struct Index{
  int32_t depthIndex;
  int32_t colIndex; 
  int32_t rowIndex;
} Index, *PIndex;

template <class Type>
class ThreeDimensionalArray{
private:
   const std::string IndexOutOfRangeException = "IndexOutOfRangeException at ";
   const std::string MemoryAllocationError = "Memory allocation error !"; 
   Type* fBaseAdress;  
   int32_t fColCount;
   int32_t fRowCount;
   int32_t fDepthSize;
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

   void MemoryAllocationCheck(){
      if (!fBaseAdress) 
        std::runtime_error(MemoryAllocationError.c_str());
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
     MemoryAllocationCheck();
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

  Type & operator[](PIndex index){ 
     Type* elementAdress = CalculateElementAdress(index->depthIndex, index->colIndex, index->rowIndex);
     return *elementAdress;
  }
  
  void ReSize(int32_t depthSize, int32_t colCount,int32_t rowCount){
     SetDepthAndColAndRowCount(depthSize,colCount,rowCount);
     CalculateMemorySize();
     fBaseAdress = (Type*)realloc(fBaseAdress,fMemorySize);
     MemoryAllocationCheck();
  }

  int32_t GetColCount() const{
    return fColCount;
  }

  int32_t GetRowCount() const{
    return fRowCount;
  }

  int32_t GetDepthSize() const{
    return fDepthSize;
  }

  int32_t GetMemorySize() const{
    return fMemorySize;
  }
};

#endif