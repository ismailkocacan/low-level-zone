#include "DynamicArray.hpp"


typedef struct _Data {
	int value;
} Data, *PData;


int main() {

	iso::DynamicArray<int> myArray(2);
	myArray.SetElement(0, 10);
	myArray.SetElement(1, 20);
	int value = myArray.GetElement(0);
	value = myArray.GetElement(1);
	value = *myArray.GetElementPointer(1);

	myArray[0] = 31;
	myArray[1] = 32;
	value = myArray[0];
	value = myArray[1];
	 
	
	try
	{
		iso::DynamicArray<Data> dataArray(2);
		dataArray[0].value = 50;
		dataArray[1].value = 36;
		dataArray.Serialize("data_array.bin");

		iso::DynamicArray<Data> dataArray2("data_array.bin");
		int test = dataArray2.GetElement(0).value;
		int test2 = dataArray2.GetElement(1).value;

		value = dataArray[0].value;
		PData dataPtr = dataArray.GetElementPointer(0);
		PData dataPtr2 = dataArray.GetElementPointer(30);
	}
	catch (const std::exception& e)
	{
		std::cout << e.what() << std::endl;
	}
	return 0;
}