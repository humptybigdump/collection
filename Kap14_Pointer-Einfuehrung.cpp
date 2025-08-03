#include <iostream>
using namespace std;

int main (){
	int*a; //Deklaration Pointer a, der auf int zeigt
	
	//Dereferenzierung
	int i=3;
	a = &i;
	cout << "*a = "<< *a << endl;
	cout << "1. 	i = "<< i << endl;
	*a= 5;
	cout << "2. 	i = "<< i << endl;
	
	//Wertzuweisungen:
	int *b=a;
	cout << "*b = "<< *b << endl;
	
	/* 
	//Fehler! c Pointer, der auf double zeigt; Variable i vom Typ int
	double *c=&i
	cout << "*c = "<< *c << endl;
	*/
	
	// Pointer und Felder:
	int x[4];
	int y, z;
	for (int i=0; i<4; i++){
		x[i]=i;
	}
	int *p;
	p=x;		//Pointer auf die erste Feldkomponente
	y =*x; 		// entspricht y=x[0];
	z= *(x+2);  //entspricht z=x[2];
	cout << "y = "<< y << endl;
	cout << "z = "<< z << endl;

	return 0;
}
