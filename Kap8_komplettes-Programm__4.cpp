#include <iostream>
#include <cmath>
using namespace std;

double mypower(double,int); 	// Deklariert mypower

double square(double x) { 		// Definiert square
return mypower(x,2); 			// Aufruf von anderem Unterprogramm mypower
}

// Unterprogramm zur Berechnung von x hoch n mit ganzer Zahl n
double mypower(double x, int n) {
int m = abs(n); // Absolut-Betrag von n
double y = 1.0;
// Berechnung von x hoch m (m in IN)
for(int i=0; i<m; ++i) 
	{y *= x;}
if (n >= 0) {
return y;
} 
else {		//wenn die Hochzahl negativ ist -> Bruch
return 1.0 / y;
}

}



// Beginn des Hauptprogrammes:
int main (){
	double a;
	int b;
	double erg;
	
	cout << "Bitte Basis eingeben "<< endl;
	cin >> a;
	cout << "Bitte Exponent eingeben" << endl;
	cin >> b;
	
	cout << "das Ergebnis lautet " << mypower (a,b) << endl;
	cout << "das Quadrat des Exponenten lautet " << square (b) << endl;
	
	
	return 0;
}
