#include <iostream>
using namespace std;

int main (){
	// zu sortierendes Feld anlegen:
	int l = 10; 		//10 Elemente in dem Feld s
	int s[l];
	cout << "Bitte die 10 Komponenten eingeben: "<< endl;
	for (int i=0; i<l; i++){
		cin >> s[i];
	}
	
	//Ablauf des Selection Sort Algorithmus:
	int temp,imin,min;
	for (int i = 0; i <= l-2; i++) { 	// Minimum suchen
		min = s[i]; 						// a[i] ist erstes Minimum,
		imin = i; 							// dazu den Index i merken
		for (int j= i+1; j < l; j++) {// restliche a[j] prüfen
			if (s[j] < min) { 				// a[j] < bisheriges Minimum?
				min = s[j]; 				// Minimum speichern
				imin = j; 					// und dessen Index
			}
		}
		// Elemente i und imin vertauschen,
		temp = s[imin]; // ein Element zwischenspeichern,
		s[imin] = s[i]; // dann das andere umspeichern
		s[i] = temp; // temp umspeichern
	}
	
	//Ausgabe
	cout << "das sortierte Feld: "<< endl;
	for (int i=0; i<l; i++){
		cout << s[i]<< "   ";
	}
	
	return 0;
}
