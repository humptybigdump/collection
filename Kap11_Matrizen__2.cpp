#include <iostream>
using namespace std;

int main (){
	
	//Initialisierung der Matrizen a und b:
	double a [3][3];
	double b [3][3];
	for (int i=0; i<9; i++){
		for (int j=0;j<9;j++){
			a[i][j]= i+j;
			b [i][j]= 10-(i+j);
		}
	}
	
	//Matrixaddition:
	double add [3][3];
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
		add[i][j] = a[i][j] + b[i][j];
		}
	}
	
	//skalares Vielfaches:
	double skalar [3][3];
	int multi = 2;
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
		skalar[i][j] = a[i][j]*multi;
		}
	}
	
	//Matrixprdukt:
	double prod [3][3];
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
		// Skalarprodukt berechnen
		double skp = 0.0;
			for (int k = 0; k < 3; k++) { 
				skp += a[i][k] * b[k][j];
			}
		prod[i][j] = skp;
		}
	}
	
	//Matrix- Vektor- Produkt:
	double skprod=0.0;
	double vec[3] = {2,2,2};
	double MVProd [3];
	for (int i = 0; i < 3; i++) {
		double skprod = 0.0;
		for (int k = 0; k < 3; k++) {
			skprod += a[i][k] * vec[k];
		}
		MVProd[i] = skprod;
	}
	
	//Transponierte Matrx:
	double at[3][3]; 
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			at[j][i] = a[i][j];
		}
	}
	
	//Beispiel für die Ausgabe:
	cout << "die transponierte Matrix von A lautet "<< endl;
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			cout<< at[i][j] << "  ";
		}
		cout << endl;
	}
	
	return 0;
}
