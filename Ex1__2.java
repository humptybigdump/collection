class Ex1 {

    /*@ normal_behavior
      @  requires n >= 0;
      @  ensures \result == (\product int i; 1 <= i < n + 1; i);
      @  measured_by n;
      @  assignable \strictly_nothing;
      @*/
    public static long factorialRec(long n) {
        if (n == 0) {
            return 1;
        }
        return n * factorialRec(n - 1);
    }

    /*@ normal_behavior
      @  requires n >= 0;
      @  ensures \result == (\product int i; 1 <= i < n + 1; i);
      @  assignable \strictly_nothing;
      @*/
    public static long factorialIt(long n) {
        long res = 1;
        /*@ loop_invariant 1 <= i <= n + 1;
          @ loop_invariant res == (\product int j; 1 <= j < i; j);
          @ decreases n - i + 1;
          @ assignable \strictly_nothing;
          @*/
        for (int i = 1; i <= n; i++) {
            res *= i;
        }
        return res;
    }

    public static void main(String[] args) {
        // It is usually a good idea to test your code before trying to verify it! ;-)
        System.out.println(factorialIt(0));
        System.out.println(factorialIt(1));
        System.out.println(factorialIt(2));
        System.out.println(factorialIt(3));
        System.out.println(factorialIt(4));
        System.out.println(factorialIt(5));
        System.out.println(factorialIt(6));
    }
}
