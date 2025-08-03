#include <iostream>
using namespace std;


int main (){
	int n, m=3;
	cout <<"Bitte die Laenge eingeben: "<< endl;
	cin >> n;
	
	// 1D Feld:
	double *feld;
	feld = new double[n]; // n double reservieren durch "new",
	//Initialisierung:
	for (int i=0; i<n; i++){
		feld [i]=i;
	}
	
	//2D Feld: Feld: mxn
	double **Feld = new double*[m]; //erzeugt m Zeilenanfaenge
	for (int i = 0; i < m; i++) {		 //erzeugt i-te Zeile
		Feld[i] = new double[n]; 
	}
	//Initialisierung des dynamischen Feldes:
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < n; j++) {
			Feld[i][j] = i+j; 
		}
	}
	
	cout << endl << Feld[m-1][n-1]<< endl;
	cout << feld [0];
	
	//Speicher wieder freigeben:
	//1D Feld:
	delete[] feld;
	//2D Feld:
	for (int i=0; i<n; i++){		//Reihenfolge umgekehrt wie bei Allokation!
		delete[] Feld [i];
	}
	delete[] Feld;


	return 0;
}
