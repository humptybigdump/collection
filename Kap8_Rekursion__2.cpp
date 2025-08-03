#include <iostream>
using namespace std;

double recpower(double x, int n) {
if (n > 0) {
return recpower(x,n-1) * x; // Rekursion
} else {
return 1; // Nicht rekursiver Zweig
}
}

int main (){
	double b;
	int e;
	cout << "Bitte Basis und Exponent eingeben " << endl;
	cin >> b >> e;
	
	cout << "das Ergebnis lautet "<< recpower (b,e) << endl;
	
	return 0;
	
}
