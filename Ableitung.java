package generalpack;

public class Ableitung extends Basis {
	private void methode4() {
		System.out.println("methode4");
		}
	protected void methode5() {
		System.out.println("methode5");
		}
	public void methode6() {
		System.out.println("methode6");		
		}
		
	public void FrageB(){
		//methods from parent class accessible as they are inherited (except the private method) 
		//this.methode1();
		this.methode2();
		this.methode3();
		
		//own methods accessible from within the class
		this.methode4();
		this.methode5();
		this.methode6();
	}
	
	public void FrageC(){
		//method from parent class called on a parent class object (private not accessible from this class)
		//super.methode1();
		super.methode2();
		super.methode3();
		
		//the following methods are from the child class, therefore cannot be applied to a parent class object
		//super.methode4();
		//super.methode5();
		//super.methode6();
	}

}