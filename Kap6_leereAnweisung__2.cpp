#include <iostream>
using namespace std;

//fehlende Klammern als Fehlerquelle:
int main() {
	int a=1, b=5, c=10;
	
	if (a > b);		// if-Anweisung mit ; beendet
	cout << "a ist groesser b" << endl; //steht nicht in if-Anweisung, wird immer ausgefuehrt-> FEHLER!

	if (c >= b)
		cout << "c ist groesser gleich b" << endl; //gehoert zu der if-Anweisung

	for (; a < 3; a++)
		cout << a << endl;		//ist Teil der for-Schleife
	cout << a + 2 << endl;		//ist kein Teil der for-Schleife, wird erst danach ausgefuehrt

	return 0;
}
