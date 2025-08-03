import java.util.Scanner;

public class ExerciseOne {
    public static void main(String[] args) {
        String prefix = args[0];
        String suffix = args[1];

        Scanner scanner = new Scanner(System.in);
        String input = scanner.nextLine();

        String result = prefix + input + suffix;

        System.out.println(result);
        scanner.close();
    }
}