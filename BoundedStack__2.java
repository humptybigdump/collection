class BoundedStack {

    private /*@ spec_public @*/ final int stack[];
    private /*@ spec_public @*/ int pos = 0;
    
    /*@ public invariant pos >= 0;
      @ public invariant pos <= stack.length;
      @*/

    /*@ normal_behaviour
      @  requires capacity >= 0;
      @  ensures \fresh(this);
      @  ensures stack.length == capacity;
      @  ensures pos == 0;
      @  assignable stack;
      @*/
    BoundedStack(int capacity) {
        this.stack = new int[capacity];
    }

    /*@ normal_behaviour
      @  requires pos > 0;
      @  ensures \result == stack[pos];
      @  ensures pos == \old(pos) - 1;
      @  assignable pos;
      @
      @ also
      @
      @ exceptional_behavior
      @  requires pos <= 0;
      @  signals_only IndexOutOfBoundsException;
      @  assignable \nothing;
      @ */
    int pop() {
        if ( pos <= 0) {
            throw new IndexOutOfBoundsException();
        }
        return stack[--pos];
    }

    /*@ normal_behaviour
      @ requires pos < stack.length;
      @ ensures stack[\old(pos)] == value;
      @ ensures pos == \old(pos) + 1;
      @ assignable stack[pos], pos;
      @
      @ also
      @
      @ exceptional_behavior
      @ requires pos >= stack.length;
      @ signals_only IndexOutOfBoundsException;
      @ assignable \nothing;
      @*/
    void push(int value) {
        if (pos >= stack.length) {
            throw new IndexOutOfBoundsException();
        }
        stack[pos++] = value;
    }

    /*@ normal_behaviour
      @  ensures \result == pos;
      @  assignable \strictly_nothing;
      @*/
    int size() {
        return pos;
    }

    /*@ normal_behaviour
      @  ensures \result == stack.length;
      @  assignable \strictly_nothing;
      @*/
    int capacity() {
        return stack.length;
    }
}

class StackClient {

    /*@ normal_behaviour
      @   requires \invariant_for(bs);
      @   ensures true;
      @*/
    void test(BoundedStack bs, int v, int w) {

        if(bs.capacity() >= bs.size() + 2) {
            bs.push(v);
            bs.push(w);
            int p1 = bs.pop();
            assert p1 == w;
            int p2 = bs.pop();
            assert p2 == v;
        } else {
            try {
                bs.push(v);
                bs.push(w);
                assert false;
            } catch(IndexOutOfBoundsException ex) {
                // expected!
            }
        }
    }
}
