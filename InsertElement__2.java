public class InsertElement {

    /*@ public normal_behavior
      @ requires ...
      @ ensures ...
      @ assignable ...
      @*/
    public static void copyRange(int[] src, int[] dest, int from, int to, int offset) {
        /*@ loop_invariant ...
          @ decreases ...
          @ assignable ...
          @*/
        for (int j = from; j < to; ++j) {
            dest[j + offset] = src[j];
        }
    }

    /*@ public normal_behavior
      @ requires ...
      @ ensures ...
      @ assignable ...
      @*/
    public static int[] insert(int[] arr, int index, int newElement) {
        int[] newArr = new int[arr.length + 1];
        copyRange(arr, newArr, 0, index, 0);
        newArr[index] = newElement;
        copyRange(arr, newArr, index, arr.length, 1);
        return newArr;
    }
}

