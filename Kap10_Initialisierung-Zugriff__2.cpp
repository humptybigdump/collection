#include <iostream>
using namespace std;

int main (){
	//Initialisierung von Felder:
	int a [4]={2,4,6,8};
	int b[]={1,2,3,};
	// 2-dimensionales Feld:
	int c [2][4]= {{2,4,6,8},{1,2,3,}}; //letzte Stelle wird automatisch mit einer 0 aufgefuellt
	
	
	//Zugriff auf Feldkomponenten:
	for (int i=0; i<4; i++){
	cout << "a["<<i<<"] = " << a[i]<< endl;
 	}
 	
 	// 2 for-Schleifen, da das Feld 2 dimensional:
 	cout << "das 2- dimensionale Feld c: "<< endl;
 	for (int i=0; i<2; i++){
 		for (int j=0; j<4; j++){
 			cout << c[i][j] << "   ";
		 }
		 cout << endl;
	 }
	 
	 /* 
	 //Vorsicht, dies ist nicht moeglich! Die Initierung beginnt immer bei 0 und endet bei n-1 (in diesem Fall 3)
	 cout << a[4];
	 cout << a[-1];
	 */
	 
	 /*
	 Vorsicht! Eine solche Schreibweise ist nicht erlaubt und fuehrt zu einer Fehlermeldung:
	 cout << c[1,4];
	 */
 	
 	
 	
 	
 	
 	return 0;
}
