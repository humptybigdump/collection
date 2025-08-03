#include <iostream>
#include <cmath>
using namespace std;

//Funktion von der Nullstelle ermittelt werden soll:
double func (double x){
	return exp(x)+x;
}

//Ableitung von "func":
double dfunc (double x){
	return exp(x)+1;
}

int main (){

double x, e, xn;
int n,N;
cout << "Bitte Anfangswert, Genauigkeit und maximale Schrittzahl eingeben" << endl;
cin >> x;
cin >> e;
cin >> N;

    
for (int n=0; ;n++) {
	xn=x;
    x = x - (func(x) / dfunc(x));
	cout << "die aktuelle Naeherung lautet " << x << endl;

	//Abbruchkriterium: die Genauigkeit wurde erreicht
	if (fabs(x-xn)< e*fabs(xn)) {
		cout << "die geforderte Genauigkeit wurde erreicht " << endl;
		break; }

	//Abbruchkriterium: die maximale Schrittzahl wurde ueberschritten
	if (n==N) {
		cout << "die maximale Schrittzahl wurde ueberschritten " << endl;
		break; }
}


cout << "die Nullstelle liegt bei " <<x<< endl;

return 0;
}


