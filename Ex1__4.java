class Ex1 {

    /*@ normal_behavior
      @  requires n >= 0;
      @  ensures \result == (\product long i; 1 <= i && i < n + 1; i);
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
      @  ensures \result == (\product long i; 1 <= i && i < n + 1; i);
      @  assignable \strictly_nothing;
      @*/
    public static long factorialIt(long n) {
        long res = 1;
        /*@ loop_invariant res == (\product long k; 1 <= k && k < i; k);
          @ loop_invariant 1 <= i && i <= n + 1;
          @ decreases n + 1 - i;
          @ assignable \strictly_nothing;
          @*/
        for (long i = 1; i < n + 1; i++) {
            res *= i;
        }
        return res;
    }
}
