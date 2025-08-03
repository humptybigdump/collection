#include <iostream>
using namespace std;

int main() {
	int zahl = 1;
	

	do {					
		zahl *= 5;
		cout << zahl << endl;
	} while (zahl <= 1000);
	//Verbundanweisung wird solange ausgefuehrt bis logischer Ausdruck danach false ist

	return 0;
}