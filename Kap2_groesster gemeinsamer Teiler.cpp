#include <iostream>
using namespace std;

int main() {
	// a und b einen Wert zuweisen
	int a, b;
	cout << "Bitte gib die Zahl a ein " << endl;
	cin >> a;
	cout << "Bitte gib die Zahl b ein " << endl;
	cin >> b;

	// while Schleife wird solange durchgefuehrt, bis entweder a oder b nicht mehr groe�er als 0
	while (a > 0 && b > 0) {

		if (a > b) {
			a = a - b;			//soll durchgefuehrt werden, wenn a>b
		}
		else {
			b = b - a;			//soll durchgefuehrt werden, wenn a nicht gr��er b
		}
	}

	cout << "der groesste gemeinsame Teiler lautet ";

	// Pruefen ob b gleich 0
	if (b == 0) {
		cout << a << endl;		// die Konsole gibt die Zahl a aus, wenn b=0
	}
	else {
		cout << b << endl;		// die Konsole gibt die Zahl b aus, wenn b nicht 0 (also a=0)
	}

	return 0;
}
