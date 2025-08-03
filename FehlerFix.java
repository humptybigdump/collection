public class FehlerFix {
  public static void main (String[] args) {
    int j = 1;
    final int c;
    c = 0;
    int a[] = new int[] {1};
    System.out.println(c);
    for (int i = 0; i < j; i++) {
      System.out.println(a[i]);
    }
    int b = 0;
  }
}
