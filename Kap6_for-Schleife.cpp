#include <iostream>
using namespace std;


int main() {

	cout << "Konsolenausgabe der 1. for-Schleife" << endl;
	// i existiert nur in der Schleife
	//hochzaehlen von 4 bis 9
	for (int i=4; i < 10; i++) {
		cout << i << endl;
	}


	cout << endl << "Konsolenausgabe der 2. for-Schleife" << endl;
	int j=5; // Definition von j vor der Schleife
	for (; j < 10; ++j) {	//Initialisierungteil faellt weg
		cout<< j<< endl;
	}
	//moeglich,da j auch ausserhalb der Schleife existiert
	cout << "der Zaehler lautet " << j << endl;


	cout << endl << "Konsolenausgabe der 3. for-Schleife" << endl;
	for (int k = 1; ; k*=2) {	//Abbruchsteil faellt weg
		if (k <= 32) {
			cout<< k<< endl;
		}
		else { break; }		// Ersatz-Abbruch, falls if-Bedingung false
	} 


	cout << endl << "Konsolenausgabe der 4. for-Schleife" << endl;
	for (int l = 0; l < 21; ) {		//Updateteil faellt weg
		l += 5;						// Ersatz-Update
		cout << l << endl;
	}



	cout << endl << "Konsolenausgabe der 5. for-Schleife" << endl;
	int m = 10;				//Ersatz Initialisierungsteil
	for (; ; ) {			// Leeres for, die 2 Semikolons duerfen nicht fehlen!
		if (m > 5) {		//Ersatz Abbruchsteil
			cout << m-- << endl;	//Ersatz Updateteil (m--)
		}
		else { break; }
	} 
	

	return 0;
}
