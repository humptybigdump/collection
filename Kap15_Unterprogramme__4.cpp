#include <iostream>
#include <math.h>
using namespace std;

double* grideval(double (*f)(double), double* grid, int Ng) {
	double* fg = new double[Ng];
	for(int i = 0; i < Ng; i++) {
		fg[i] = (*f)(grid[i]);	
	}
	return fg;
}


int main (){
	//Berechnung der Werte einer uebergebenen Funktion f (in diesem Fall sin) auf einem Gitter
	int n=5;
	double*a = new double [5];
	for (int i=0; i<n; i++){
		a[i]= i*0.5*M_PI;		//M_PI steht fuer Pi=3,1415...
	}
	for (int i= 0; i<n; i++){
		cout << a [i]<< endl;
	}
	
	double*b = new double [5];
	b=grideval(sin,a,n); 	
	
	cout << "das Ergbnis lautet "<< endl;
	for (int i= 0; i<n; i++){
		cout << b [i]<< endl;		// b[i]=sin (a[i])
	}
	
	return 0; 
}
