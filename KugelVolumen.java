import java.util.*;
/* Berechnung des Kugelvolumens
Eingabe: r (Kugelradius)
Ausgabe: v (Kugelvolumen)*/

public class KugelVolumen{
  public static void main (String [] args){
    Scanner in = new Scanner(System.in);
    System.out.print("Bitte Kugelradius eingeben: ");
    double r = in.nextDouble();
    double v = 4.0 / 3.0 * Math.PI * r * r * r;
    System.out.println("Das Volumen betraegt v = " + v);
  }
}
//in.useLocale(Locale.GERMAN);
