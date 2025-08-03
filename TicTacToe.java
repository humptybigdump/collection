import Prog1Tools.IOTools;

/*  x | 0 | x
    ---------
    0 | X | 0
    ---------
    X |   |   */

public class TicTacToe
{ 
	public static char[][] spielfeld;
	public static char winningChar = ' ';
	
    public static void main (String [] args)
    {
        //Starte neues Spiel
        System.out.println("Spielen wir ein neues Spiel!");
        System.out.println("Sie fangen an!");
        setUpNewGame();
        printSpielfeld();

		boolean won = false;
		while(!won){
	        askUserForInput();
	        //printSpielfeld();
			won = checkWinConditon();
			if(!won){
	        	computerPlaceO();
			}
	        printSpielfeld();
	        won = checkWinConditon();
		}

		System.out.println("Spiel zuende! Gewonnen hat: " + winningChar);

        printSpielfeld();
    }

    public static boolean checkWinConditon(){
    	boolean result = false;
		 
    	//horizontal
    	for(int i = 0; i<spielfeld.length; i=i+1){
    		if(spielfeld[i][0] == spielfeld[i][1] && 
    		       spielfeld[i][1] == spielfeld[i][2] &&  
    		         spielfeld[i][0] != ' '){
    		    result = true;
    		    winningChar = spielfeld[i][0];
    			System.out.println("1");
    		}
    	}
    	//vertikal
    	for(int i = 0; i<spielfeld.length; i=i+1){
    		if(spielfeld[0][i] == spielfeld[1][i] && 
    		       spielfeld[1][i] == spielfeld[2][i] &&  
    		         spielfeld[0][i] != ' '){
    		    result = true;
    		    winningChar = spielfeld[0][i];
    		    System.out.println("2");
    		}
    		
    	}

    	//diagonal
    	if(spielfeld[0][0] == spielfeld[1][1] && 
    		       spielfeld[1][1] == spielfeld[2][2] &&  
    		         spielfeld[0][0] != ' '){
    		    result = true;
    		    winningChar = spielfeld[0][0];
    		    System.out.println("3");
    	}

    	if(spielfeld[0][2] == spielfeld[1][1] && 
    		       spielfeld[1][1] == spielfeld[2][0] &&  
    		         spielfeld[0][2] != ' '){
    		        result = true;
    		       winningChar = spielfeld[1][1];
    	}
    	
    	return result;
    }

    public static void computerPlaceO(){
		int y = (int)(Math.random()*3);
    	int x = (int)(Math.random()*3);
		boolean postionOK = xUndYOK(x,y);

		while(!postionOK){
    		y = (int)(Math.random()*3);
    		x = (int)(Math.random()*3);
			postionOK = xUndYOK(x,y);
		}

		spielfeld[y][x] = 'O';
    }

    public static void askUserForInput(){
    	
    	int y = IOTools.readInt("Zeile? ");
    	int x = IOTools.readInt("Spalte? ");
		boolean postionOK = xUndYOK(x,y);

		while(!postionOK){
			System.out.println("Postion ungültig! Bitte erneut eingeben!");
    		y = IOTools.readInt("Zeile? ");
    		x = IOTools.readInt("Spalte? ");
			postionOK = xUndYOK(x,y);
		}

		spielfeld[y][x] = 'X';
    }

    public static boolean xUndYOK(int x, int y){
    	boolean result = true;

		if(x<0||x>2||y<0||y>2){
			result = false;

		}else{
			if ( (spielfeld[y][x] == 'X') || (spielfeld[y][x] == 'O') ){
				result = false;
			}
		}
    	return result;
    }

    public static void printSpielfeld(){
    	System.out.println("___________________________________________");
    	for(int i = 0; i<spielfeld.length; i=i+1){
    		for(int j = 0;j<spielfeld[i].length; j=j+1){
    			System.out.print(" " + spielfeld[i][j] + " ");
    			if(j<spielfeld.length-1){System.out.print("|");}
    	    }
    	    
    	    System.out.println();
    	    if(i<spielfeld.length-1){System.out.println("-----------");}
    	}
    	System.out.println("___________________________________________");
    }

    public static void setUpNewGame(){
        spielfeld = new char[3][3];
        spielfeld[0][0] = ' ';
        spielfeld[1][0] = ' ';
        spielfeld[2][0] = ' ';
        spielfeld[0][1] = ' ';
        spielfeld[1][1] = ' ';
        spielfeld[2][1] = ' ';
        spielfeld[0][2] = ' ';
        spielfeld[1][2] = ' ';
        spielfeld[2][2] = ' ';
    }
}