#include <iostream>
using namespace std;

int main (){
	int Orginal [2][3];
	for(int i = 0; i < 2; i++) {
		for(int j = 0; j < 3; j++) {
			Orginal [i][j]= 4-i;
		}
	}
	
	//tiefe Kopie:
	int Kopie [2][3];
	for(int i = 0; i < 2; i++) {
		for(int j = 0; j < 3; j++) {
			Kopie [i][j]= Orginal [i][j];
		}
	}
	
	// Ausgabe auf der Konsole
	cout << "die Matrix- Kopie: " << endl;
	for(int i = 0; i < 2; i++) {
		for(int j = 0; j < 3; j++) {
			cout << Kopie [i][j] << "  ";
		}
		cout << endl;
	}
	
	return 0;
}
