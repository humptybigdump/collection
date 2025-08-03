#include <iostream>
using namespace std;

//Programm zur Berechnung der Quersumme (vgl Folie 28)
int main() {
	int n, summe = 0;
	cout<< "n = ?"<< endl;
	cin >> n;

	//Verbundanweisung solange ausgefuehrt, bis logischer Ausdruck false
	while (n > 0) {
		summe += n % 10; // Zehnerrest summieren
		n /= 10; // Ganzzahlige Division
	}

	cout<< "Quersumme = "<< summe<< endl;

	return 0;
}
