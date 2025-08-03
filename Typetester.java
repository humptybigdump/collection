/**
* How to use: 
*    Typetester typeTest = new Typetester();
*    typeTest.printType( 2.0 / 4 );
*
* @author Stefan Karch
* @date 15.11.22
*/

class Typetester {
    void printType(long x) {
        System.out.println(x + " is a long");
    }
    void printType(byte x) {
        System.out.println(x + " is a byte");
    }
    void printType(int x) {
        System.out.println(x + " is an int");
    }
    void printType(float x) {
        System.out.println(x + " is a float");
    }
    void printType(double x) {
        System.out.println(x + " is a double");
    }
    void printType(char x) {
        System.out.println(x + " is a char");
    }
    void printType(String x) {
        System.out.println(x + " is a String");
    }    
    void printType(boolean x) {
        System.out.println(x + " is a boolean");
    }
}
