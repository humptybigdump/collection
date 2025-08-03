#include <iostream>				//Includefile
#include <cmath>				//Includefile um auf mathematische Standardfunktionen zugreifen zu koennen
using namespace std;			//Namespace, zB fuer die Ein- und Ausgaberoutinen (cin, cout)
int a = 10;						// file-globale Variable

double pluszehn (double x){
	return x+a; 				// Zugriff auf file-globale Variable moeglich
}

int main (){
	double x=0.0;

	for (int i=0; i<10; i++){	//Beginn des Blocks: i ist lokale Variable
		x+=i;
	}							//Ende des Blocks: i ist nur bis hierhin bekannt.
	/*cout << i << endl;		// Fehler!
	*/

	cout << "x= " << x << endl;
	cout << "x+a= "<< pluszehn (x) << endl;

	int a = 15;			//lokale Variable mit demselben Namen wie die file-globale Variable
	cout << "a(Hauptprogramm) = "<< a << endl;
	cout << "a(file-globale Variable) = "<< ::a << endl;	// Verwendung des Scope-Operators


	//Statische Variablen:
	for (int i = 1; i <= 3; i++) {
		static double m = 3;		//statische Variable m bleibt bestehen
		double n = 3;				//wird bei jedem Durchlauf neu generiert
		++m;
		++n;
		cout <<"m= "<< m << "   ";
		cout <<"n= "<< n << endl;
	}

	return 0;
}
