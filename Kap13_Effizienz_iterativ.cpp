//Untersuchung der Effizienz von Sortieralgorithmen:
//Bubble Sort (iterativer Sortieralgorithmus -> durchschnittlicher Rechenaufwand O(n^2))
#include <iostream>
#include <time.h>
#include <cmath>
using namespace std;

int main() {

	//Definition des zu sortierenden Feldes:
	cout << "Bitte die Laenge des Feldes eingeben"	<< endl; //zB n>1000
	unsigned int alength;
	cin >> alength;
	double feld[alength];

	for (int i = 0; i < alength; i++) {
		feld[i] = (alength/2) - i;
	}

	//Rechenzeit mesen:
	double time = 0.0, tstart;		//Zeit Vaiablen
	tstart = clock();				//Beginn der Messung

	double zaehl = 0.0;

	//Beginn des Algorithmus:
	double temp;
	for (int i = 1; i < alength; i++) { // Unsort. Teil = i-1...Ende
		for (int j = alength - 1; j >= i; j--) { // Rueckwaerts durchlaufen
			if (feld[j - 1] > feld[j]) { // Vertausche feld[j] und feld[j-1]
				temp = feld[j];
				feld[j] = feld[j - 1];
				feld[j - 1] = temp;
				zaehl++;				//zaehlt wie oft die Werte miteinander vertauscht werden
			}
		}
	}
	cout << "das Feld ist sortiert: " << feld[0] << " , " << feld[alength - 1] << endl;

	//Beenden der Zeitmessung:
	time += clock() - tstart; 		//Ende der Messung
	time = time / CLOCKS_PER_SEC;  // Umscalierung in Sekunden
	cout << "Zeit = " << time << " sec." << endl;

	cout << "Anzahl der Rechenoperationen = " << zaehl << endl;		//entspricht circa n^2


	return 0;
}
