// MapPointer.cpp : Defines the entry point for the console application.

#include "stdafx.h"
#include <stdio.h>
#include <map>
#include <iostream>

using namespace std;

typedef int(*PCalculate)(int x, int y);

map<string, PCalculate> mtable;

int Sum(int x, int y)
{
	return x + y;
}

int Subtruct(int x, int y)
{
	return x - y;
}

int Multiply(int x, int y)
{
	return x * y;
}

int Divide(int x, int y)
{
	return x / y;
}

int _tmain(int argc, _TCHAR* argv[])
{
	mtable.insert(pair<string, PCalculate>("Sum", &Sum));
	mtable.insert(pair<string, PCalculate>("Subtruct", &Subtruct));
	mtable.insert(pair<string, PCalculate>("Multiply", &Multiply));
	mtable.insert(pair<string, PCalculate>("Divide", &Divide));

	int result = 0;
	result = mtable["Sum"](10, 5);
	printf("Sum %d \n", result);

	result = mtable["Subtruct"](5, 5);
	printf("Subtruct %d \n", result);

	result = mtable["Multiply"](5, 5);
	printf("Multiply %d \n", result);

	result = mtable["Divide"](5, 5);
	printf("Divide %d \n", result);

	return 0;
}

