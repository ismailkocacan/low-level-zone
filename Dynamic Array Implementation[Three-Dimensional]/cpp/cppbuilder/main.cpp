#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include "threedimensionalarray.hpp"

int _tmain(int argc, _TCHAR* argv[])
{
	ThreeDimensionalArray<int32_t> anArray(2,2,2); // 2 adet, 2 sütun, 2 satýrlýk tablo

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

    // access elements via indexer
	Index index = { 0, 0, 0 };
    int32_t val = anArray[&index];
    anArray[&index] = 31;

    system("pause");
	return EXIT_SUCCESS;
}
