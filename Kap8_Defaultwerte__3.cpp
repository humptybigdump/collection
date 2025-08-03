#include <iostream>
using namespace std;

int rechteck (int a=2, int b=4){
	return a*b;
}


int main (){
	//kein Default Wert:
	cout << "Aufruf 1:" << endl;
	cout << rechteck (6,7)<< endl;

	//Default Wert fuer b:
	cout << "Aufruf 2:" << endl;
	cout << rechteck (5)<< endl;

	//Default Wert fuer a und b:
	cout << "Aufruf 3:" << endl;
	cout << rechteck ()<< endl;


	return 0;
}
