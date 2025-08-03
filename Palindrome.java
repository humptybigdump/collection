// Name:
// Matrikelnummer:
// E-Mail-Adresse:

public class Palindrome {

    /*@ normal_behavior
      @  requires str.length > 0;
      @  ensures !(\exists int i; 0 <= i < str.length / 2; str[i] != str[str.length - i - 1]) ==> \result == str;
      @  ensures (\exists int i; 0 <= i < str.length / 2; str[i] != str[str.length - i - 1]) ==>
      @          \result.length == 2 * str.length
      @       && (\forall int i; 0 <= i < str.length; \result[i] == str[i])
      @       && (\forall int i; 0 <= i < str.length; \result[str.length + i] == str[str.length - i - 1]);
      @  assignable \nothing;
      @*/
    public static char[] makePalindrome(char[] str) {
        if (isPalindrome(str)) {
            return str;
        }
        char[] result = new char[str.length * 2];
        copyInto(str, result, 0);
        copyInto(reverse(str), result, str.length);
        return result;
    }

    /*@ ... @*/
    private static void copyInto(char[] src, char[] dest, int pos) {
        /*@ ... @*/
        for (int i = 0; i < src.length; i++) {
            dest[pos + i] = src[i];
        }
    }

    /*@  ...
      @  ensures \fresh(\result);
      @  ...
      @*/
    private static char[] reverse(char[] str) {
        char[] result = new char[str.length];
        /*@ ... @*/
        for (int i = 0; i < str.length; i++) {
            result[i] = str[str.length - i - 1];
        }
        return result;
    }

    /*@ normal_behavior
      @  requires true;
      @  ensures \result <==> !(\exists int k; 0 <= k < str.length / 2; str[k] != str[str.length - k - 1]);
      @  assignable \strictly_nothing;
      @*/
    private static boolean isPalindrome(char[] str) {
        int m = str.length / 2;
        boolean isPalindrome = true;

        /*@ loop_invariant 0 <= i <= m;
          @ loop_invariant isPalindrome <==> !(\exists int j; 0 <= j < i; str[j] != str[str.length - j - 1]);
          @ decreases m - i;
          @ assignable \strictly_nothing;
          @*/
        for (int i = 0; i < m; i++) {
            if (str[i] != str[str.length - i - 1]) {
                isPalindrome = false;
            }
        }
        return isPalindrome;
    }
}
