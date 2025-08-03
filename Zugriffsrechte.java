//For the java virtual machine: Compile the classes with the packages
//javac -d . Basis.java
//javac -d . Ableitung.java
//javac -d . Zugriffsrechte.java

//Run the class with the main method
//java meinpack/zugriffsrechte

//Antworte
//1,2,3
//2-6
//2,3
//3
//3,6

//Tabelle Zugriffsrechte java : https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html

package meinpack;
import generalpack.*;

public class Zugriffsrechte{
	public static void main(String[] args){
		Basis instanz1 = new Basis();
		Ableitung instanz2 = new Ableitung();
		
		System.out.println("Frage a:");
		instanz1.FrageA();
		System.out.println();
		
		System.out.println("Frage b:");
		instanz2.FrageB();		
		System.out.println();
		
		System.out.println("Frage c:");
		instanz2.FrageC();	
		System.out.println();
		
		System.out.println("Frage d:");
		//methods in the Basis class (Instanz1 is a basis object).
		//only the public one can be accessed, the protected cannot be accessed as it is in another package.
		//instanz1.methode1();	
		//instanz1.methode2();
		instanz1.methode3();
		
		//methods from the Ableitung class
		//as they are from the child class, they cannot be used on a parent object
		//instanz1.methode4();
		//instanz1.methode5();
		//instanz1.methode6();
		System.out.println();
		
		System.out.println("Frage e:");
		//all methods can be applied to an object of the child class, but only the public one can be accessed from here.
		//instanz2.methode1();	
		//instanz2.methode2();
		instanz2.methode3();
		//instanz2.methode4();
		//instanz2.methode5();
		instanz2.methode6();
		
	}
}