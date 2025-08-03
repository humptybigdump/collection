public class SearchAndRotate {
    /*@ public normal_behaviour
      @ assignable ...
      @ ensures ...
      @*/
    public static int search(int a[], int val) {
        int res = -1;
        /*@ loop_invariant ...
          @ assignable ...
          @ decreases ... @*/
        for (int i = 0; i < a.length; i++) if (a[i] == val) res = i;
        return res;
    }

    /*@ public normal_behaviour
      @  requires 0 <= len && len <= a.length;
      @  assignable ...
      @  ensures ...
      @*/
    public static void rotate(int a[], int len) {
        int[] b = new int[a.length];
        int i = 0;
        /*@ loop_invariant ...
          @  assignable ...
          @  decreases ... @*/
        for (i = 0; i < len; i++) b[i] = a[a.length - len + i];
        /*@ loop_invariant ...
          @  assignable ...
          @  decreases ... @*/
        for (i = len; i < a.length; i++) b[i] = a[i - len];
        /*@ loop_invariant ...
          @ assignable ...
          @ decreases ... @*/
        for (i = 0; i < a.length; i++) a[i] = b[i];
    }

    /*@ public normal_behaviour
      @ requires 0 <= to && to < a.length;
      @ assignable \everything;
      @ ensures a[to] == val;
      @*/
    public static void rotateValueToPos(int a[], int val, int to) {
        final int pos = search(a, val);
        if (pos != -1) rotate(a, pos <= to ? to - pos : a.length + to - pos);
        else a[to] = val;
    }
}
