public class Kap8_ZuweisungUndVergleich {
    public static void main(String args[]) {
    
    boolean f;
    int x = 1;
    int y = 2;
    
    f = (x == y);   //Vergleichsoperator
    //Wenn x und y gleich sind wird das ausgegeben, genauso wenn sie ungeleich sind
    if (f == true){
        System.out.println ("Gleich");
    }
        else {
            System.out.println ("Ungleich");
        }
    
    //Gleichsetzen
    x = y;  //Zuweisungsoperator
    f = (x == y);   //Vergleichsoperator
    
    //Wenn x und y gleich sind wird das ausgegeben, genauso wenn sie ungeleich sind
    //Ist jetzt immer gleich, da x der Wert von y zugeschrieben wurde.
    if (f == true){
        System.out.println ("Gleich");
    }
        else{
            System.out.println ("Ungleich");
        }
    
        }
}