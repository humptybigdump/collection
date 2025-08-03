package generalpack;

public class Basis{
	private void methode1() {
		System.out.println("methode1");
		}
	protected void methode2() { 
		System.out.println("methode2");
		}
	public void methode3() { 
		System.out.println("methode3");
		}
	
	public void FrageA(){
		//own class methods accessible from within the class
		this.methode1();
		this.methode2();
		this.methode3();
		
		//class methods from child class not applicable to parent class object
		//this.methode4();
		//this.methode5();
		//this.methode6();
	}

}