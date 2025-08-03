// 2147483647 
class Loop2Start {

    /*@ public normal_behavior
      @  requires n >= 0;
      @  ensures \result == n * n;
      @*/
    public int m(int n) {
	int i = 0; 
	int r = 0;

        /*@ loop_invariant  i>=0 && i<=n && 2*r == i*(i+1);
	  @ assignable i, r;
	  @ decreases n-i;
          @*/
       while(i < n){
         i = i + 1;
         r = r + i;
       }
       r = r + r - n;

       return r;
    }


}