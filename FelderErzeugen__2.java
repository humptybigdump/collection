public class FelderErzeugen {
  public static void main(String[] args) {
    // create an array (declaration)
    int[][] a;
    // create an array of dimension 2x3 (instantiation)
    int[][] b = new int[2][3];
    // create an array (initialization)
    char[][] c = new char[][]{{'a','b','c'},{'d','e','f'}};
    // alternative initialization
    char[][] d = {{'a','b','c'},{'d','e','f'}};
    // dimension must not be the same
    char[][] d = {{'a'},{'b','c'}};
  }
}
