public class Kap4_Zeilenumbrueche{
    
    public static void main (String [] args){
        
        System.out.print ("Dies ist ei");   // .print() bewirkt keinen Zeilenumbruch
        System.out.println ("n einzeiliger String");
        System.out.println ("Dies ist ein ");   // .println() bewirkt einen Zeilenumbruch
        System.out.println ("zweizeiliger String"); 
        
        System.out.println ("Dies ist ei" +
                            "n einzeiliger String.");   // bewirkt keinen Zeilenumbruch!
        System.out.println ("Dies ist ein \nzweizeiliger String."); //Durch \n expliziter Zeilenumbruch (s.4.4)
    }
}

