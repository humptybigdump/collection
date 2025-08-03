public class Kap9_Maximum {
    public static void main(String args[]) {
    
    int x = 21, y = 12, max;
    
    /*Bestimmung des Maximums: Ist x groesser, so ist x das Maximum
    In allen anderen Faellen (also auch bei Gleichheit!) ist das Maximum y
    */
    if (x > y){ //Klammerung waere nicht notwendig, macht Programm aber oft uebersichtlicher
      max = x;  
    }
    
        else{   //Klammerung auch hier nicht notwendig, da nur eine Anweisung folgt
           max = y; 
        }
        
    System.out.println (max);
    }
}