/*
 * Projekt Partikelsimulation
 * Semester: WiSe 24/25
 * Author: uebung IAM  
 * Datum: 25.11.2024
 * 
 * Ziel: Schritt für Schritt Implementierung eines Programms
 * zur Simulation der Trajektorie eines Partikel in einem Fluid
 * Spezialfall: Lamiare Stroemung und Kugelfoermiges Partikel
 * 
 * Erweiterung auf 2D (Wurf)
 */

import java.util.Scanner; 

public class Partikelsimulation
{
    public static void main(String[] args) {
        // Konstanten
        final double g = 9.81; // Erdbeschleunigung in m/s^2  

        // Eingaben:
        double R; // Radius des Partikel, zB. R = 0.1e-3 m; 
        double rho_p; // Dichte des Partikel, zB. rho_p = 7870 Kg/m^3 (Dichte von Eisen)
        double rho_f; // Dichte des Fluids, zB. rho_f = 1000 Kg/m^3 (Dichte von Wasser)
        double mu; //(dynamische) Viskositaet des Fluids, zB. mu = 1e-3 Kg/(m s) (Viskositaet von Wasser)

        // Einlesen der Partikel und Fluiddaten
        System.out.println("Bitte folgende Partikel und Fluiddaten eingeben: ");
        Scanner myScan = new Scanner(System.in);
        System.out.print("  - Partikelradius(in [m]): ");
        R = myScan.nextDouble();
        System.out.print("  - Partikeldichte (in [Kg/m^3]): ");
        rho_p = myScan.nextDouble();
        System.out.print("  - Fluiddichte (in [Kg/m^3]): ");
        rho_f = myScan.nextDouble();
        System.out.print("  - (Dynamische) Fluidviskositaet (in [Kg/(m s)]): ");
        mu = myScan.nextDouble();

        // Explizites Eulerverfahren:
        // Einlesen der benötigten Größen
        System.out.println("Anfangsgeschwindigkeit, Zeitschritt und Anzahl an Iterationen eingeben: ");
        System.out.print("  - Anfangsgeschwindigkeit vx0 = ");
        double vx0 = myScan.nextDouble();
        System.out.print("  - Anfangsgeschwindigkeit vy0 = ");
        double vy0 = myScan.nextDouble();
        System.out.print("  - Zeitschritt tau = ");
        double tau = myScan.nextDouble();
        System.out.print("  - Anzahl a Iterationen N = ");
        double N = myScan.nextDouble();

        // (Anfangs) Geschwindigkeit 
        double vx = vx0;
        double vy = vy0;
        double x=0;
        double y=0;
        // Hilfgrößen
        final double m_p = 4/3. * Math.PI * Math.pow(R, 3) * rho_p; // Masse des Partikels
        final double m_f = 4/3. * Math.PI * Math.pow(R, 3) * rho_f; // Masse des Fluids

        final double Fg = (m_p - m_f) * g ;// Gravitation & Auftriebskraft

        for (int i = 0; i < N; i++)
        {   
            vy += (Fg - 6*Math.PI*mu*R*vy)/m_p* tau;
            y += tau*vy;
            vx += (- 6*Math.PI*mu*R*vx)/m_p* tau; 
            x += tau*vx;
        }
        System.out.println("-> Numerische Sinkgeschwindigkeit v = [" + vx+ ","+vy+ "] m/s"); 
        System.out.println("-> Numerische Sinkposition   [" + x+ ","+y+ "] m"); 

        // Die Referenzloesung 
        double v_ref = 2./9. * (rho_p - rho_f)/mu * g * R * R;
        System.out.println("-> Analytische Sinkgeschwindigkeit v_ref = " + v_ref + " m/s");
        double rel_eps = Math.abs(vy - v_ref)/Math.abs(v_ref);
        System.out.println("-> Der relative Fehler eps = " + rel_eps*100. + " %");       
    }
}