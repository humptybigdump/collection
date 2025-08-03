
public class Main {
    public static void main(String[] args) {

        int hight = Integer.parseInt(args[0]);

        int maxLength = (hight-1) * 2 + 1;

        for (int i = 0; i < hight; i++) {
            int countStars = i * 2 + 1;
            int sideUnderscore = (maxLength - countStars) / 2;
            String currentLine = "";

            for (int j = 0; j < sideUnderscore; j++) {
                currentLine += " ";
            }
            for (int j = 0; j < countStars; j++) {
                currentLine += "*";
            }
            for (int j = 0; j < sideUnderscore; j++) {
                currentLine += " ";
            }

            System.out.println(currentLine);
        }

    }
}