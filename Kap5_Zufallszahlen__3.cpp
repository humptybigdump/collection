#include <iostream>
#include <cstdlib>		// F�r srand
#include <ctime>		// F�r time
using namespace std;

int main() {
	srand(time(NULL));	// srand initialisieren
	// W�rfelergebnis im Bereich 1 bis 6:
	cout << "Wuerfel zeigt: " << (rand() % 6 + 1) << endl;

	return 0;
}