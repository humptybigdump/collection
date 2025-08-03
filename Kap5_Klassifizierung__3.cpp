#include<iostream>
using namespace std;

int main() {
	double i = 3.0;
	double j = 4.0;
	double k = 5.0;

	bool vergleich;
	vergleich = (++i == j);
	//muss true/1 ergeben, da Inkrementoperator hoehere Prioritaet (3) als Vergleichsoperator (9)
	cout << vergleich << endl;
	
	double erg;
	erg = ++j * k;	
	// Inkrementoperator (3) hoehere Prioritaet als Multiplikativer Operator (5), deshalb erg= 5.0 * 5.0
	cout << erg << endl;
	




	return 0;
}