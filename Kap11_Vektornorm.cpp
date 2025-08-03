#include <iostream>
#include <cmath>
using namespace std;

int main (){
	
	//Initialisierung des Vektors:
	int n;
	cout << "Bitte die Dimension des Vektors eingeben: ";
	cin >> n;
	double x[n];
	cout << "Bitte die Komponenten eingeben: "<< endl;
	for (int i=0; i<n; i++){
		cin >> x[i];
	}
	
	
	// p-Norm
	cout << "Bitte einen ganzzahligen, positiven Wert fuer die Norm eingeben: ";
	int p;
	cin >> p;
	double pnorm, sum = 0.0;
	for (int i = 0; i < n; i++){
	sum += pow(abs(x[i]),p);
	}
	pnorm = pow(sum, 1.0/p); //weil der Datentyp double ist 1.0 nicht nur 1
	cout << "die p-Norm betraegt: "<<pnorm << endl;
	
	
	//Berechung der Maximumsnorm:
	double maxnorm = 0.0; 
	for (int i = 0; i < n; i++){
		if (abs(x[i]) > maxnorm){
			maxnorm = abs(x[i]);
		}
	}
	cout << "die Maximumsnorm betraegt: "<<maxnorm << endl;	
	
	return 0;
}
