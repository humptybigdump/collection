public class FloatLimits {
  public static void main(String[] args)
  {
    System.out.println(1999999F);
    System.out.println(19999999F); // 2e7
    System.out.println(19999999F==2e7); //true
    // overflow
    System.out.println(19999998F+3F-3F); // !=19999998.0
    // auloeschung
    System.out.println( 19999998F); //  19999998.0
    System.out.println(-19999997F); // -19999996.0
    System.out.println(19999998F-19999997F); // != 1

  }
}
