#include <iostream>
using namespace std;

int positiv (int x){
	if (x>0){
		return 1;		//return, falls x postitv
	}
	else {
		return 0;		// return, falls x <= 0
	}
	//in jedem Zweig ein Return, sonst Fehler!
}


// Beginn des Hauptprogrammes:
int main (){
	int a, b;
	cout << "Bitte geben sie zwei ganze Zahlen ein "<< endl;
	cin >> a >> b;
	
	a= positiv (a);		//Unterprogramm wird mit der Variablen a aufgerufen
	b = positiv (b);	// Unterprogramm wird mit der Variablen b aufgerufen
	
	if (a==1){
		cout << "die erste Zahl ist positiv " << endl;
	}
	
	if (b==1){
		cout << "die zweite Zahl ist positiv " << endl;
	}
	
	return 0;
	
}
