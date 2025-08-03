// 2147483647 
class Loop2 {

    /*@ public normal_behavior
      @  requires n >= 0;
      @  ensures \result == n * n;
      @  diverges true;
      @*/
    public int m(int n) {
	int i = 0; 
	int r = 0;
        /*@ loop_invariant
	  @    i>=0 && 2*r == i*(i + 1) && i <= n;
	  @ assignable i, r;
          @*/
       while(i < n){
         i = i + 1;
         r = r + i;
       }
       r = r + r - n;

       return r;
    }


}