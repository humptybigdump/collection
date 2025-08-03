#include <iostream>
using namespace std;

//Deklaration von Klassen:

class Student{
	private: 				// data hiding, Zugriff nur innerhalb der Klasse
		double note;
	public:					//oeffentlicher Zugriff erlaubt
		string name;		//Elementliste
		int matrikel;
		int * studentAnzahl;

		static int count; 	//Klassenvariable, gekennzeichnet durch static;

		//Deklaration und Definition von Konstruktor 1 mit 3 Argumenten:
		Student (string n, int m, double no){
			name = n;
			matrikel = m;
			note = no;
		};
		//Deklaration von Konstruktor 2 mit 2 Argumenten:
		Student (string , int );

		//Deklaration und Definition von Standardkonstruktor (ohne Argumente):
		Student (){
			name = "no name";
			matrikel = 0;
		}

		friend string bestanden(Student);	//friend Funktion

		double zeige_note (){	//Instanzmethode:
			return note; 		//Definition innerhalb der Klasse
		}

		string zeige_name (){
			return name;
		}

		int zeige_matrikel (); 	//Instanzmethode, Definition ausserhalb der Klasse


		//Destruktor um dynamisch allokierten Speicher freizugeben (Pointer auf studentAnzahl)
		~Student(){
			delete[] studentAnzahl;
		}
};
//Definition der Klassenvariablen:
int Student::count = 3;

//Definition von Konstruktor2:
Student:: Student(string n, int m){
		name = n;
		matrikel = m;
	}

//friend Funktion kann auf private zugreifen:
string bestanden (Student s){
	if (s.note <= 4.0){
		return "bestanden";
	}
	else {
		return "nicht bestanden";
	}
}

//Defintion der Instanzmethode
int Student::zeige_matrikel (){
	return matrikel;
}


//Hauptprogramm:
int main (){
	Student s ("A", 123, 1.3); //neue Instanz durch Konstruktor 1 mit 3 Instanzvariablen

	Student s1("B", 345); // neue Instanz durch Konstuktor2
	Student s2("C", 678); //Konstuktor2 mit 2 Argumenten, also 2 Instanzvariablen

	Student s3; 		//neue Instanz ohne Argumente, also Standardkonstruktor

	//Aufruf der Instanzmethoden:
	cout <<"Matrikelnummer von s1: "<< s1.zeige_matrikel () << endl;
	cout << "Name von s3: "<< s3.zeige_name () << endl;
	cout <<"Note von s: "<< s.zeige_note () << endl;

	//Aufruf der friend-Funktion:
	cout << "der Student " << s.zeige_matrikel() << " hat " << bestanden(s) << endl;
	
	return 0;
}
