// Name:           ...
// Matrikelnummer: ...
// E-Mail-Adresse: ...

class Permutation {

    ////////////////////////// iterative implementation

    /*@ normal_behavior
      @  requires ...
      @  ensures ...
      @  ensures \fresh(\result);
      @  assignable ...
      @*/
    public static int[] permuteIt(int[] vals, int[] perm) {
        int[] res = new int[vals.length];
        /*@ loop_invariant ...
          @ decreases ...
          @ assignable ...
          @*/
        for (int i = 0; i < vals.length; i++) {
            res[i] = vals[perm[i]];
        }
        return res;
    }

    ////////////////////////// recursive implementation

    /*@ normal_behavior
      @  requires ...
      @  ensures ...
      @  ensures \fresh(\result);
      @  assignable ...
      @*/
    public static int[] permuteRec(int[] vals, int[] perm) {
        int[] res = new int[vals.length];
        permuteRec0(vals, perm, res, 0);
        return res;
    }

    /*@ normal_behavior
      @  requires ...
      @  ensures ...
      @  assignable ...
      @  measured_by ...
      @*/
    private static void permuteRec0(int[] vals, int[] perm, int[] res, int idx) {
        if (0 > idx || idx >= res.length)
            return;
        res[idx] = vals[perm[idx]];
        permuteRec0(vals, perm, res, idx+1);
    }


    ////////////////////////// sanity checks

    /*@ normal_behavior
      @  requires true;
      @  ensures \result.length == 4;
      @  ensures \result[0] == 2 && \result[1] == 3 && \result[2] == 0 && \result[3] == 1;
      @  assignable \nothing;
      @*/
    public int[] testIt() {
        int[] vals = {0, 1, 2, 3};
        int[] perm = {2, 3, 0, 1};
        return permuteIt(vals, perm);
    }

    /*@ normal_behavior
      @  requires true;
      @  ensures \result.length == 4;
      @  ensures \result[0] == 2 && \result[1] == 3 && \result[2] == 0 && \result[3] == 1;
      @  assignable \nothing;
      @*/
    public int[] testRec() {
        int[] vals = {0, 1, 2, 3};
        int[] perm = {2, 3, 0, 1};
        return permuteRec(vals, perm);
    }

    ////////////////////////// "main" proof obligation

    /*@ normal_behavior
      @  requires vals.length == perm.length;
      @  requires (\forall int i; 0 <= i < perm.length; 0 <= perm[i] < vals.length);
      @  ensures true;          // the interesting "postcondition" is in the assertion!
      @  assignable \nothing;
      @*/
    public static void lemmaItEqRec(int[] vals, int[] perm) {
        int[] r = permuteIt(vals, perm);
        int[] s = permuteRec(vals, perm);
        //@ assert (\forall int i; 0 <= i < r.length; r[i] == s[i]);
    }
}
