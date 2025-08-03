#include <iostream>
#include <cmath>
using namespace std;

int main (){
	
	//Initialisierung der Matrix:
	int n;
	cout << "Bitte die Dimension der rechteckigen Matrix eingeben: ";
	cin >> n;
	double x[n][n];
	cout << "Bitte die Komponenten eingeben: "<< endl;
	for (int i=0; i<n; i++){
		for (int j=0; j<n;j++){
			cin >> x[i][j];
		}
		cout << endl;
	}
	
	//P-Norm der Matrix berechnen:
	cout << "Bitte einen ganzzahligen, positiven Wert fuer die Norm eingeben: ";
	int p;
	cin >> p;
	
	double pnorm, sum = 0.0;
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
		sum += pow(abs(x[i][j]), p);
		}
	}
	pnorm = pow(sum, 1.0/p); 
	cout << "die p-Norm betraegt: "<<pnorm << endl;
	
	//Berechung der Spaltensummennorm:
	double snorm = 0.0;
	for (int j = 0; j < n; j++) {
		double sum = 0.0;
		for (int i = 0; i < n; i++) {	//Berechnung der Summe einer Spalte
			sum += abs(x[i][j]);
		}	
		if (sum > snorm) {				//Vergleich der Spaltensummennormen um maximale zu ermitteln
			snorm = sum;
		}
	}
	cout << "die Spaltensummennorm betraegt: "<<snorm << endl;
	
	
	return 0;
}
	
