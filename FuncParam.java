/* Illustration of default parameters in Java
   Author: Mathilde Wu, Dec 2020 */
   
/* ! in python for example it is possible to pass a formal parameter of a function like:

	def addition( a, b=4){
		return (a+b)
	}

	-> Here 4 will be the default value of b if the value of b is not specified
	-> for example addition(2) = 2+4=6
	->addition(2,5) = 2+5=7
	
	but in java this syntax does not work, instead, one can overload the function
	This means here writing two functions, that will not take the same number of parameters
*/

public class FuncParam{
	
	//function with 2 parameters
	static int addition( int a, int b){
		return a+b;
	}
	
	//same function with only one parameter
	static int addition (int a){
		return addition(a,4); //we can retake the previous function and put b=4. It acts as if 4 was the default value of b when no b is specified
	}	
	
	public static void main(String []args){
		System.out.println(addition(1,4)); // = 5
		System.out.println(addition(1));  // = 5
	}
	
}