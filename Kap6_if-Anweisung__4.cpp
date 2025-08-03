#include <iostream>
using namespace std;

int main() {
	int x, y;
	cout << "Bitte x,y eingeben:" << endl;
	cin >> x;		
	cin >> y;


	if (x <= y) {					//true Teil
		cout << "x ist kleiner gleich y" << endl;		//Anweisung
	}

	else {							//false Teil
		cout << "x ist groesser als y" << endl;			//Anweisung
	}


	if (x < 0) {										
		//Verbund aus 2 Anweisungen: geschweifte Klammern notwenig!
		x = x * (-1);
		cout << "der Betrag von x lautet " << x << endl;
	}
	//einseitig bedingte Anweisung, kein else-Teil

	
	if (y < 0)			//geschweifte Klammern fehlen
		y *= (-1);		// diese Anweisung wird ausgefuehrt, wenn if-Bedingung true
	cout << "der Betrag von y lautet " << y << endl;	//steht nicht in der if-Anweisung, wird immer ausgefuehrt

	

	return 0;
}