public class Kap17_Fibonacci{
    
    //Methode zur Berechnung der i-ten Fib.-Zahl 
    static int fibonacci (int i) {
    if (i <= 1)
        return 1;   // nicht rekursiver Zweig
    else
        return fibonacci(i-1) + fibonacci(i-2);     // zwei(!) rekursive Aufrufe
}
    
    public static void main (String [] args){
        
        int i = 5;
        //Aufruf zur Methode
        double f = fibonacci (i);
        System.out.println ("Die " + i + "-te Fibonacci-Zahl ist " + f);
    }
}


\begin{eqnarray*}
        f_0 &=& 1,\\
        f_1 &= &1, \\
        f_i &=& f_{i-1} + f_{i-2} \quad \mbox{f"ur } i \ge 2.
        \end{eqnarray*}





        \codeblock{\noindent

static int fibonacci (int i) \{\\
        \quad if (i <= 1) \\
        \quad \quad return 1; \hfill\makebox[6cm][l]{\cmt{// nicht rekursiver Zweig}}\\
        \quad else  \\
        \quad \quad return fibonacci(i-1) + fibonacci(i-2);\cmt{// zwei(!) rekursive Aufrufe}\\
        \}\\
public static void main (String [] args){\\
        \quad int i = 5;\cmt// Aufruf zur Methode\\
        \quad System.out.println ("Die " + i + "-te Fibonacci-Zahl ist " + fibonacci));\\
        \}\\

        }










public static void main (String [] args) \{\\
        \quad int i = 5; \quad \hfill\makebox[6cm][l]{\cmt{// Aufruf zur Methode}}\\
        \quad System.out.println (i + "-te Fibonacci-Zahl = " + fibonacci)); \\
        \}\\


static int fibonacci (int i) \{\\
        \quad if (i <= 1) \\
        \quad \quad return 1; \hfill\makebox[6cm][l]{\cmt{// nicht rekursiver Zweig}}\\
        \quad else  \\
        \quad \quad return fibonacci(i-1) + fibonacci(i-2); \\
        \quad \quad \cmt{// zwei(!) rekursive Aufrufe}\\
        \}\\