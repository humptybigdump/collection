#include <iostream>
#include <cmath>
using namespace std;

class Complex {
	public:
		double re; // Realteil der komplexen Zahl
		double im; // Imaginaerteil
		//Konstruktor:
		Complex (double a, double b){
			re = a;
			im = b;
		}
		friend Complex operator+(Complex, Complex);
		friend Complex operator*(Complex, Complex);

};

//Ausgabeoperator
ostream& operator<<(ostream& os, Complex& x){
	if (x.im >= 0){
		os << x.re << " + " << x.im << "*i";
	}
	else{
		os << x.re << " - " << -x.im << "*i";
	}
	return os;
}

//Komplexe Addition:
Complex operator+ (Complex a, Complex b){
	return Complex(a.re+b.re, a.im+b.im);
}

//Komplexe Multiplikation:
Complex operator*(Complex a, Complex b) {
	return Complex(a.re*b.re-a.im*b.im, a.re*b.im+a.im*b.re); // Aufruf Konstruktor
}


int main (){
	//Instanzen:
	Complex z1 (1.3, 5.6);
	Complex z2 (2.5, 7.0);
	Complex z3 (3.2, -1.0);

	//Ausgabe mit Ausgabeoperator:
	cout << "z1 = " << z1 << endl;
	cout << "z2 = " << z2 << endl;
	cout << "z3 = " << z3 << endl;

	//Additions- und Multiplikationsoperator:
	Complex erg1 = z1 + z2;
	Complex erg2 = z1 * z3;
	cout << "z1 + z2 = " << erg1 << endl;
	cout << "z1*z3 = " << erg2 << endl;

	return 0;
}
