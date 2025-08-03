public class Felderb2 {
  public static void main (String[] args) {  
    int[] a = new  int[] {1, 3, 4, 2, 3};
    int  e = 0;
    for( int i = 0; i < a.length; i++ ) {
      e += Math.abs(a[i]);
      System.out.println(e);
    }
  }
}
