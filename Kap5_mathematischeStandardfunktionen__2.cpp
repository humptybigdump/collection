#include<iostream>
#include<cmath>				// Mathematische Funktionen einbeziehen
using namespace std;
int main() {
	double x;				// Deklaration
	cout<< "Geben Sie ein x ein" << endl; // Text zur Eingabe
	cin >> x;				// Eingabeaufforderung
	cout<< "2*sin(x) = "<< 2 * sin(x) << endl; // Rechnung und Ausgabe
	return 0;
}