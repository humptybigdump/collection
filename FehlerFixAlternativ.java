public class FehlerFixAlternativ {
  public static void main (String[] args) {
    int j;
    int c = 0;
    c++; //c+=1;
	System.out.println("c="+c);
    int[] a;
    a = new int[]{1,3,44,2,8};
	j = a[0]+4; //1+4
    System.out.println("j="+j); //j=5
    for (int i = 0; i < j; i++) { //i++   i+=1
      System.out.println("a["+i+"]="+a[i]);
    }
    a[0] = 0;
	for (int i = 0; i < j; i++) { //i++   i+=1
      System.out.println("a["+i+"]="+a[i]);
    }
	//System.out.println(a); funktioniert nicht, nicht möglich ein Feld in Java direkt zu drucken 
	//-> Man muss eine for Schleife erstellen für die Werte zu lesen (wie oben)
  }
}
