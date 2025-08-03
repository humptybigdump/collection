
public class Main {
    public static void main(String[] args) {
        int upperBound = Integer.parseInt(args[0]);


        int result1 = 0;
        for (int i = 1; i <= upperBound; i++) {
            result1 += i;
        }

        int result2 = 0;
        int i = 1;
        while (i <= upperBound) {
            result2 += i;
            i++;
        }

        System.out.println(result1);
        System.out.println(result2);


    }
}