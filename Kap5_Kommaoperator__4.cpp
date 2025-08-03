#include <iostream>
using namespace std;

int main() {
	double a, b, c;

	// Sinnvoll: Zusammenfassung von Ausdruecken:
	a = 1, b = 2, c = 3;

	a++, b++, c--;		// Ergibt a=2, b=3, c=2
	// Verwirrend aber korrekt:
	c = (a = 33, b = 44); // Ergibt a=33, b=44, c=44
	// Ausgabe auf der Konsole
	cout << "a= " << a << endl;
	cout << "b= " << b << endl;
	cout << "c= " << c << endl;

	double d;
	d = 1.2345; // Vorsicht: nicht d= 1,2345!
	//Ausgabe auf der Konsole
	cout << "d= " << d << endl;

	return 0;
}