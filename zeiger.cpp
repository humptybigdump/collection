#include <iostream>
using namespace std;

void beispiel1() {
	cout << "Beispiel 1" << '\n';
	// Deklaration Zeiger
	double* dp;
	// Anlegung Speicher
	dp = new double(500.5);
	// Ausgabe der Adresse
	cout << " dp:\t"  << dp  << '\n';
	// Dereferenzierung des Pointers
	cout << "*dp:\t" << *dp << '\n';
	cout << '\n';
	
	// Freigeben des Speichers non-array Objekts mit 'delete'
	delete dp;
}

void beispiel2() {
	cout << "Beispiel 2" << '\n';
	// Deklaration Zeiger und Anlegung Speicher
	int* ip = new int(200);
	// Ausgabe der Adresse
	cout << " ip:\t"  << ip  << '\n';
	// Dereferenzierung des Pointers
	cout << "*ip:\t" << *ip << '\n';
	cout << '\n';
	
	// Freigeben des Speichers non-array Objekts mit 'delete'
	delete ip;
}

void beispiel3() {
	cout << "Beispiel 3" << '\n';
	// Deklaration Zeiger
	double* dp;
	// Anlegung Speicher
	dp = new double(9123.312312);
	// Ausgabe der Adresse
	cout << " dp:\t"  << dp  << '\n';
	// Dereferenzierung des Pointers
	cout << "*dp:\t" << *dp << '\n';
	cout << '\n';
	
	// Freigeben des Speichers non-array Objekts mit 'delete'
	delete dp;
}

void beispiel4() {
	cout << "Beispiel 4" << '\n';
	// Deklaration Zeiger und Anlegung Speicher
	int* ip = new int(5);
	// Ausgabe der Adresse
	cout << "ip:\t"  << ip  << '\n';
	// Dereferenzierung des Pointers
	cout << "*ip:\t" << *ip << '\n';
	cout << "ip[0]:\t" << ip[0] << '\n';
	cout << '\n';
	
	// Freigeben des Speichers non-array Objekts mit 'delete'
	delete ip;
}

int main() {
	beispiel1();
	beispiel2();
	beispiel3();
	beispiel4();
}