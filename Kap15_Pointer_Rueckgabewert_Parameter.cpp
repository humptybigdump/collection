#include <iostream>
using namespace std;

double summe (double*feld, int dim){ 	//Uebergabe von Pointer
	double sum = 0.0;
	for (int i = 0; i < dim; i++) {
	sum += feld[i];
	}
	return sum;
}

double* einsplus (double*feld, int dim){	// Rueckgabewert ist ein Pointer auf ein Feld
	for (int i = 0; i < dim; i++) {
	feld[i] += 1;
	}
	return feld; 
}

int main (){
	
	//Feld deklarieren als Pointer auf double:
	int n = 5;
	double feld[n];
	for (int j=0; j<n; j++){
		feld[j]=j;
	}
	//Aufruf der Funktion summe:
	double ergebnis;
	ergebnis= summe (feld,n);
	cout << "das Ergebnis lautet "<< ergebnis << endl;
	
	
	//Pointer als Rueckgabewert:
	double *feld2= einsplus (feld, n);
	cout << "das neue Feld:" << endl;
	for (int j=0; j<n; j++){
		cout <<feld2[j]<< endl;
	}
	
	return 0;
}
