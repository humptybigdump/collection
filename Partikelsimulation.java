/*
 * Projekt Partikelsimulation
 * Semester: WiSe 24/25
 * Author: uebung IAM  
 * Datum: 11.11.2024
 * 
 * Ziel: Schritt für Schritt Implementierung eines Programms
 * zur Simulation der Trajektorie eines Partikel in einem Fluid
 * Spezialfall: Lamiare Stroemung und Kugelfoermiges Partikel
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

        // Die Referenzloesung 
        double v_ref = 2./9. * (rho_p - rho_f)/mu * g * R * R;
        System.out.println("-> Die Referenzgeschwindigkeit v_ref = " + v_ref + " m/s");

        // Gravitation & Auftriebskraft
        double Fg = 4./3. * (rho_p - rho_f) * g * Math.PI * R * R * R;
        System.out.println("-> Die Gravitations und Auftriebskraft F_g = " + Fg + " N");

        // Stroemungswiderstand 
        double Fs = 6*Math.PI*mu*R*v_ref;
        System.out.println("-> Der Stroaemungswiderstand Fs = " + Fs + " N");
        
    }
}