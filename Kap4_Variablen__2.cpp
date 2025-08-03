#include <iostream>
using namespace std;

int main() {

	//Definition und Deklaration von Variablen:
	int j = 10;			//Definition von j = 10

	int m, k;			//Deklaration der Variablen m und k


	//Initialisierung:
	int i(6);			//implizit
	k = 7;			//explizit
	m = k + 2;		// okay, da k vor m initialisiert

	cout << "die Variable i hat den Wert " << i << endl;
	cout << "die Variable k hat den Wert " << k << endl;
	cout << "die Variable m hat den Wert " << m;
	return 0;
}