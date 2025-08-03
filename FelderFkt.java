// Author: A. Mink, Sept.2019
public class FelderFkt{
  static int getMax(int[] n) {
    int max = 0;
    for( int i = 0; i < n.length; i++ ) {
      if( n[i] > max ) {
        max = n[i];
      }
    }
    return max;
  }
  static int getl1(int[] n)
  {
    int res = 0;
    for( int i = 0; i < n.length; i++ ) {
      res += Math.abs(n[i]);
    }
    return res;
  }
  static double getl2(int[] n)
  {
    double res = 0;
    for( int i = 0; i < n.length; i++ ) {
      res += Math.pow(Math.abs(n[i]), 2);
    }
    return Math.sqrt(res);
  }
  public static void main (String [] args){
    int[] a = new int[] {4,2,3};
    System.out.println(getMax(a));
    System.out.println(getl1(a));
    System.out.println(getl2(a));
  }
}
