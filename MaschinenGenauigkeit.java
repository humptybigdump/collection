public class MaschinenGenauigkeit {
  public static void main(String[] args)
  {
    // false, doubles are not equal
    System.out.println(3.1d==3.0d);
    // false, wrong datatype
    System.out.println(3.1d==3.1f);
    // true, cast double to float
    System.out.println((float)3.1d==3.1f);
    // true, cast double/float to int
    System.out.println((int)3.1d==(int)3.1f);
  }
}
