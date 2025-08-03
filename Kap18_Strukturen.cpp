#include <iostream>
using namespace std;

struct Complex {
	double re;			//Elementliste
	double im;
}z;					//Variablenliste

int main (){
	//Initialisierung von struct- Groessen:
	Complex z = {3.5, 0.7};
	
	//Vereinbarung neuer Instanzen
	Complex z2= {1.0, 3.7};
	
	//Element-Zugriff:
	cout << "z= "<< z.re <<" + i* "<< z.im << endl;
	cout << "z2= "<< z2.re <<" + i* "<< z2.im << endl;
	
	//-> Operator:
	Complex* zp = &z;
	cout << "zp= "<< zp->re <<" + i* "<< zp->im << endl;
	
	return 0;
}
