#include <iostream>
using namespace std;


int main (){
	double Feld [3][3][3];
	
	
	// 3 for- Schleifen um mehrdimensionales Feld zu initialisieren
	for(int i = 0; i < 3; i++) {
		for(int j = 0; j < 3; j++) {
			for(int k = 0; k < 3; k++) {
				Feld [i][j][k]= i+2*j+3*k;
			}
		}	
	}
	
	//Zugriff
	cout << "Feld [0][0][0]= " << Feld [0][0][0] << endl;
	cout << "Feld [0][1][2]= " << Feld [0][1][2] << endl;
	cout << "Feld [1][1][1]= " << Feld [1][1][1] << endl;
	cout << "Feld [2][2][2]= " << Feld [2][2][2] << endl;
	
	return 0;
}
