// Name:           ...
// Matrikelnummer: ...
// E-Mail-Adresse: ...

class ArraySet {
  int[] values;

  /*@ normal_behavior
    @  requires ...
    @  ensures ...
    @  assignable \nothing;
    @*/
  ArraySet() {
    values = new int[0];
  }

  /*@ normal_behavior
    @  requires ...
    @  assignable ...
    @
    @ also
    @
    @ normal_behavior
    @  requires ...
    @  ensures ...
    @  assignable ...
    @*/
  void add(int v) {
    if (contains(v))
      return;
    int[] newValues = new int[values.length + 1];
    copyValues(values, newValues);
    newValues[newValues.length-1] = v;
    values = newValues;
  }

  /*@ normal_behavior
    @  ...
    @*/
  static void copyValues(int[] src, int[] dest) {
    /*@ ...
      @*/
    for (int i = 0; i < src.length; i++) {
      dest[i] = src[i];
    }
  }
  /*@ normal_behavior
    @  ensures \result == (\exists int i;
    @             0 <= i < values.length; values[i] == v);
    @  assignable \strictly_nothing;
    @  accessible values, values[*];
    @*/
  boolean contains(int v) {
    /*@ loop_invariant 0 <= i <= values.length
      @    && (\forall int j; 0 <= j < i; values[j] != v);
      @ decreases values.length - i;
      @ assignable \strictly_nothing;
      @*/
    for (int i = 0; i < values.length; i++) {
      if (values[i] == v)
        return true;
    }
    return false;
  }

  /*@ normal_behavior
    @  ensures true;
    @*/
  public static void main(String[] args) {
    ArraySet as = new ArraySet();
    //@ assert as.values.length == 0;
    //@ assert !(\exists int x; as.contains(x));

    as.add(5);
    //@ assert as.values.length == 1;
    //@ assert as.contains(5);

    as.add(5);
    //@ assert as.values.length == 1;
    //@ assert as.contains(5);

    as.add(7);
    //@ assert as.values.length == 2;
    //@ assert as.contains(5);
    //@ assert as.contains(7);
  }
}
