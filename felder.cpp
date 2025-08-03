#include <iostream>
using namespace std;

void beispielFeld1(int m, bool showSolution) {
	// Deklaration Zeiger und Anlegung Speicher
	double *A = new double[m];
	// Zuweisung von Werten
	cout << "Inhalt des Feldes:\n";
	for(int i=0; i<m; ++i) {
		A[i] = i*5+10;
	}
	// Ausgabe der Werte
	for(int i=0; i<m; ++i) {
		cout << A[i] << ' ';
	}
	
	// Fragen
	cout << '\n';
	cout << '\n';
	
	cout << "Was gibt A zurück?\n";
	if(showSolution)
		cout << A << '\n';
	
	cout << "Was gibt A[3] zurück?\n";
	if(showSolution)
		cout << A[3] << '\n';
	
	cout << "Was gibt *A zurück?\n";
	if(showSolution)
		cout << *A << '\n';
	
	cout << "Was gibt *A+1 zurück?\n";
	if(showSolution)
		cout << *A+1 << '\n';
	
	cout << "Was gibt *(A+1) zurück?\n";
	if(showSolution)
		cout << *(A+1) << '\n';
	
	cout << '\n';
	
	// Freigeben des Speichers von Feldern mittels 'delete[]'
	delete[] A;
}

void beispielFeld2(int m, int n, bool showSolution) {
	// Erzeugt m double-Pointer
	double **A = new double*[m];
	
	//Erzeugt m×n double
	for(int i=0; i<m; ++i) {
		A[i] = new double[n];
	}
	
	// Initialisierung
	for(int i=0; i<m; ++i) {
		for(int j=0; j<n; ++j) {
			A[i][j] = ((i*3)+j)*2;
		}
	}
	
	// Ausgabe
	cout << "So sieht das Feld aus:\n";
	for(int i=0; i<m; ++i) {
		for(int j=0; j<n; ++j) {
			cout << A[i][j] << '\t';
		}
		cout << '\n';
	}

	// Fragen
	cout << '\n';
	
	cout << "Was gibt A zurück?\n";
	if(showSolution)
		cout << A << '\n';
	
	cout << "Was gibt A[1] zurück?\n";
	if(showSolution)
		cout << A[1] << '\n';
	
	cout << "Was gibt *A zurück?\n";
	if(showSolution)
		cout << *A << '\n';
	
	cout << "Was gibt *A+1 zurück?\n";
	if(showSolution)
		cout << *A+1 << '\n';
	
	cout << "Was gibt *(A+1) zurück?\n";
	if(showSolution)
		cout << *(A+1) << '\n';
	
	
	cout << "Was gibt **A zurück?\n";
	if(showSolution)
		cout << **A << '\n';
	
	cout << "Was gibt **A+1 zurück?\n";
	if(showSolution)
		cout << **A+1 << '\n';
	
	cout << "Was gibt *(*A+1) zurück?\n";
	if(showSolution)
		cout << *(*A+1) << '\n';
	
	cout << "Was gibt **(A+1) zurück?\n";
	if(showSolution)
		cout << **(A+1) << '\n';
	
	cout << '\n';
	
	// Freigeben des Speichers von Feldern mittels 'delete[]'
	// ACHTUNG: Umgekehrte Reihenfolge!
	for(int i=0; i<m; ++i) {
		delete[] A[i];
	}
	delete[] A;
}

int main() {
	int m=5, n=3;
	//cout << "Bitte Feldlänge eingeben: " << '\n';
	//cin >> m;
	//cin >> n;
	
	beispielFeld1(m, true);
	beispielFeld2(m, n, true);
}