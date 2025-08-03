public class Kap14_Zugriff {
    
    public static void main(String[] args) {
        
        double[][][] quader = new double[4][2][3];   // 4x2x3 = 24 Speicherstellen fuer double-Zahlen
        quader[1][1][2] = 3.14; //Wertzuschreibung fuer diese Komponente
        System.out.println (quader [1][1][2]);
        
        double[][] quadrat = {{ 1, 2 },{ 3, 4 }};   // 2x2 Matrix, alle Werte explizit festgelegt
        System.out.println(quadrat[0][0]);
        
        double[][] dreieck = {{ 1 },{ 2, 3 },{ 4, 5, 6 }};  // Dreiecksmatrix
        dreieck[2][2] = dreieck[0][0] + 6;  //Veraenderung des Eintrags durch Zugriff auf anderen; aus 6 wird 1+6=7
        System.out.println (dreieck [2][2]);
       
       
        double[][] mat = new double [3][4]; //erzeugt 3 x 4 - Matrix, wird solange nicht festgelegt mit 0-Eintaegen gefuellt

        for (int i = 0; i < mat.length; ++i){   // alle Zeilen
            for (int j = 0; j < mat[i].length; ++j){    // Spalten
                 System.out.println(mat[i][j]); // die Werte ausgeben
            } 
        } 
        
     }
}