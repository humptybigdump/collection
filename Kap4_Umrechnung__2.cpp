#include <iostream>
using namespace std;

// Beginn des Hauptprogramms
int main() {
	// celsius und fahrenheit reelle Zahlen, deshalb der Datentyp double
	double celsius, fahrenheit; 
	celsius = 30.0;
	fahrenheit = 1.8 * celsius + 32.0;
	//Ausgabe von Text und der Variable auf der Konsole
	cout << "Temperatur in Fahrenheit = " << fahrenheit << endl;
	return 0;
}