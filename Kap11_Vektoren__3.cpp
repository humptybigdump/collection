#include <iostream>
using namespace std;

int main (){
	// Initialisierung von Vektoren:
	int a [3]={1,2,3};
	int b [3]={4,4,4};
	int c [2]={6,10};
	int addition [3];
	int skalar [3];
	
	int lambda = 5;
	int skprod = 0;
	
	//Vektoraddition a+b:
	for (int i=0; i<3; i++){
		addition [i]= a[i]+b[i];
	}
	
	//Skalares Vielfaches: a*lambda
	for (int i=0; i<3; i++){
		skalar [i]= a[i]*lambda;
	}
	
	//Skalarprodukt mit Vektor a und b
	for (int i=0; i<3; i++){
		skprod += a[i] * b[i];
	}
	
	
	//Sonderfall: Unterschiedliche Dimensionen der Vektoren:
	//Addition von den Vektoren a[3] und c[2]:
	int sonderf [2]; 
	//Ergebnisvektor mit dem Minimum der Dimensionen:
	for (int i=0; i<2; i++){
		sonderf [i]= a[i]+c[i];
	}
		
	
	
	//Ausgabe der Ergebnisse:
	cout << "die Ergebnisse lauten: "<< endl;
	cout << "Vektoraddition: "<< endl;
	for (int i=0; i<3; i++){
		cout << addition [i] << "   ";
	}
	
	cout << endl << "Skalares Vielfaches: "<< endl;
	for (int i=0; i<3; i++){
		cout << skalar [i] << "   ";
	}
	
	cout << endl << "das Skalarprodukt: "<< skprod << endl;
	
	cout << "Vektoraddition, wenn Dimensionen verschieden: "<< endl;
		for (int i=0; i<2; i++){
			cout << sonderf [i] << "   ";
		}
	
	
	return 0;
}
