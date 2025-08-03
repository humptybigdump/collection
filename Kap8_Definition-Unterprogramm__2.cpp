#include <iostream>
using namespace std;


// 1. Unterprogramm vom Datentyp int
// der Rueckgabewert ist also auch vom Datentyp int
int zehnmal (int x){
	return x*10;
}


// 2. Unterprogramm vom Datentyp double
// 2 formale Parameter werden uebergeben, muessen vom Datentyp double sein
// Rueckgabewert ist ebenfalls vom Datentyp double
double multi (double y, double z){
	return y*z;
}


// Beginn des Hauptprogrammes:
int main (){
	
	int i=3;
	int erg1;
	double j=24.6, k=2.25;
	double erg2;
	
	//Aufruf 1. Unterprogramm:
	erg1 = zehnmal (i);
	
	//Aufruf 2. Unterprogramm:
	erg2 = multi (j,k);
	
	cout << "die Ergebnisse lauten "<< endl <<erg1 << endl << erg2;
	
	
	
	return 0;
}
