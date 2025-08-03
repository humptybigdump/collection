public class Kap11_ZugriffAufFelder {

    public static void main(String[] args) {
        
        //Feld vom Typ char anlegen, Speicherstellen mit Vokalen fuellen
        char [] vokale = {'a', 'e', 'i', 'o', 'u'};
        
        //Feldelemente ausgeben
        for (int i = 0; i < vokale.length; ++i){ //Zugriff auf Laenge ueber .length
            System.out.println(vokale[i]);
        }
            
        // ein Feld mit 4 Komponenten
        String[] farbe = {"Kreuz","Pik","Herz","Karo"};
        
        // Zugriffe auf Feldelemente
        System.out.println(farbe[0]); 
        System.out.println(farbe[3]); 
        
        /* Nicht erlaubte Zugriffe:
        System.out.println(farbe[-1]); // nicht definiert
        System.out.println(farbe[4]); // 4 > 3 !
        System.out.println(farbe[farbe.length]); // Maximaler Index ist length-1
        */
    }
}