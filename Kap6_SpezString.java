public class Kap6_SpezString {
    public static void main(String args[]) {
        
        String m = "Max" + " Maier";
        System.out.println (m); // + verbindet beide Strings
        
        String s = 50 + 30 + "abc" + 50 + 30;       
        System.out.println (s);   //Nach dem String wird + immer als Stringkonketation aufgefasst
        
        String s1 = 50 + 30 + "abc" + (50 + 30); 
        System.out.println (s1);    //Klammerung priorisiert die Addition
    }
}