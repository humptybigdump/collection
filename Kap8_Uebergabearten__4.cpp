#include <iostream>
using namespace std;

int einsplus(int x){		//Pass by value, Uebergabe als Wertparameter
	return ++x;				//wirkt sich nur innerhalb des Unterprogrammes aus
}

void swap (int& y, int& z){	//Pass by reference, Uebergabe des Speicherortes
	int hilf = y; 			// wirkt sich auch ausserhalb des Unterprogrammes aus
	y = z; 
	z = hilf;
}


int main (){
	
	int a=1, b;
	b= einsplus(a);
	cout << "der Wert fuer a lautet "<< a << endl;
	cout << "der Wert fuer b lautet "<< b << endl;
	
	int c=5, d=10;
	swap(c,d);
	cout << "der Wert fuer c lautet "<< c << endl;
	cout << "der Wert fuer d lautet "<< d << endl;
	
	
	return 0;
}
