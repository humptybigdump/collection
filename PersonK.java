public class PersonK {
  private String name;
  public int year;
  public char gender;
  // Konstruktor
  PersonK(String name_, int year_, char gender_) {
    name = name_;
    year = year_;
    gender = gender_;
  }
  
  PersonK(){
/*	name="Santa Klaus";
	year=2000;
	gender='w';
	*/
  }

  public static void main( String[] args) {
    PersonK arbeiter = new PersonK("Horst", 1966, 'm');
  //  System.out.println(arbeiter.name);
	PersonK test = new PersonK();
    System.out.println(test.name);	
	test.name="santa klaus";
	System.out.println(test.name);	

  }
}

