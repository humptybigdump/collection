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
	
	//Ablauf vom Insertion Sort Algorithmus:
	// Bereich 0 bis i-1 ist sortiert, Rest unsortiert
	for (int i=1; i < l; i++){
		//das naechste nicht einsortierte Element auswaehlen:
		int temp = s[i]; 		
		int j = i-1; 
		
		// suche rueckwaerts nach Position für temp
		while (j >= 0 && s[j] > temp){ 
		s[j+1] = s[j]; 
		j--;
		}
		// temp an richtiger Position eintragen
		s[j+1] = temp;
	}
	
	
	//Ausgabe
	cout << "das sortierte Feld: "<< endl;
	for (int i=0; i<l; i++){
		cout << s[i]<< "   ";
	}
	
	return 0;
}
