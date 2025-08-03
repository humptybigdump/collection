#include <iostream>
using namespace std;

int main() {
	int x;
	cout << "Bitte geben sie eine Zahl von 1-6 ein" << endl;
	cin >> x;

	switch (x) {
		case 1: cout << "die Anweisung fuer 1 wird ausgefuehrt"<< endl;		//falls x=1, wird ab dieser Anweisung begonnen
		case 2: cout << "die Anweisung fuer 2 wird ausgefuehrt" << endl;
		case 3: cout << "die Anweisung fuer 3 wird ausgefuehrt" << endl;
		case 4: cout << "die Anweisung fuer 4 wird ausgefuehrt" << endl; break; 
			//break Befehl verhindert die weitere Abarbeitung der Anweisungen
		case 5: cout << "die Anweisung fuer 5 wird ausgefuehrt" << endl;

		default: { //am Ende der Anweisungsfolge oder falls kein konstanter Ausdruck passt
			cout << "die default Anweisung wird ausgefuehrt" << endl;
		}
	}



	return 0;
}