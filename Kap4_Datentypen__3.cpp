#include <iostream>
using namespace std;

int main() {
	//Verschieden Datentypen:

	double wert = 123.45;
	wert = 345.67;					// Okay, wert ist bereits definierte Variable
	double quad = wert * wert;		// Okay, wert ist initialisiert

	int wertint;					// Deklaration Variable wertint
	wertint = wert;					// Fehler/Warnung: wert ist double
	cout << "der Wert der double- Variablen wert lautet " << wert << endl;
	cout << "der Wert der int-Variablen wertint lautet " << wertint << endl;
	cout << "int stellt nur ganze Zahlen dar, double hingegen reelle Zahlen"<< endl;

	return 0;
}
	