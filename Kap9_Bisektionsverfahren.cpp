#include<iostream>
#include <cmath>
using namespace std;

//Funktion bei der Nullstelle ermittelt werden soll:
double funktion (double x){
	return exp(x)+ x; 
}


int main (){
	double a, b, c;
	double epsilon = 0.00000001; 	//Genauigkeit Epsilon
	
	cout << "der untere Startwert lautet: "<< endl;
	cin >> a;	
	cout << "der obere Startwert lautet: "<< endl;
	cin >> b;	
	
	//Pruefen ob Voraussetzung erfuellt ist:
	if ((funktion(a) * funktion(b)) >= 0) {
	cout << "Voraussetzung fuer Bisektionsverfahren nicht erfuellt!!" << endl;
	return 0;
	}
	
	
	while ((b-a)>epsilon){			//Abbruchkriterium Ueberpruefung
		c=(a+b)/2;					//Intervallhalbierung
		
		if ((funktion(a) * funktion(c)) < 0){
			b=c;
		}
		
		else {
			a=c;
		}
	}
	
	
	cout << "Die Nullstelle liegt bei x = "<< c;
	
	return 0;
}
