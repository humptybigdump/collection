#include <iostream>
using namespace std;

int main() {

	double a, b;
	for (int i = 0; i < 5; ++i) {		//Anweisungen werden 5 mal durchgefuehrt
		cout << " a ? "<< endl; cin >> a;
		cout << " b ? " << endl; cin >> b;
		if (a == 0) continue;			// a=0: Naechstes i
		if (b == 0) break;				//b=0: for Schleife wird beendet
		cout << " a/b = " << a / b<< endl;
	}

	return 0;
}