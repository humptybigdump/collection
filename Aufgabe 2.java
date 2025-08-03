
public class Main {
    public static void main(String[] args) {

        for (int i = 2; i <= 100; i++) {
            boolean isPrim = true;

            for (int j = 2; j < i; j++) {
                if ( i % j == 0){
                    isPrim = false;
                }
            }

            if (isPrim) {
                System.out.println(i);
            }
        }

    }
}