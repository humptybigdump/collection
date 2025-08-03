#include <iostream>		// Bibliothek um cin und cout nutzen zu koennen
using namespace std;

int main() {

	int y, x;
	// y, x sind Variabeln vom Datentyp "int"
	cout << "Bitte gebe eine Zahl x ein " << endl;
	// Ausgabeanweisung: Text wird auf Konsole ausgegeben
	cin >> x;
	// Eingabeanweisung: die Variable x erhaelt den eingelesenen Wert
	y = 3 * x + 2;
	// Fuehre die Rechung 3x+2 aus und weise y diesen Wert zu
	cout << y << endl;
	// y wird auf der Konsole ausgegeben, endl: Zeilenumbruch

	return 0;
}
