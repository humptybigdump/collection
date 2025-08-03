#include <iostream>
using namespace std;

int main() {
	int i, g, h; 
	i = 1; g = ++i; // Praefix, ergibt g = 2 und i = 2.
	h = i++; // Postfix, ergibt h = 1 und i = 2+1 = 3.

	int a, b;
	a = b = 2;		//Mehrfachzuweisung

	a += g;			//kombinierte Zuweisung entspricht a=g+a
	b *= i;			//kombinierte Zuweisung entspricht b=b*i

	cout << "der Wert von a lautet " << a << endl;
	cout << "der Wert von b lautet " << b << endl;

	return 0;
}