#include <iostream>
#include <cstdlib>		// Für srand
#include <ctime>		// Für time
using namespace std;

int main() {
	srand(time(NULL));	// srand initialisieren
	// Würfelergebnis im Bereich 1 bis 6:
	cout << "Wuerfel zeigt: " << (rand() % 6 + 1) << endl;

	return 0;
}