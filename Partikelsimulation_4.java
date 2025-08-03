/*
 * Projekt Partikelsimulation
 * Semester: WiSe 24/25
 * Author: uebung IAM  
 * Datum: 16.12.2024
 * 
 * Ziel: Schritt für Schritt Implementierung eines Programms
 * zur Simulation der Trajektorie eines Partikel in einem Fluid
 * Spezialfall: Lamiare Stroemung und Kugelfoermiges Partikel
 * 
 * Visualisation of the moving particle
 */

 import java.awt.*;
 import java.awt.geom.Ellipse2D;
 import java.util.*;
 import javax.swing.*;
 
public class Partikelsimulation
{
    static Scanner myScan = new Scanner(System.in);
    // read vector 
    public static double[] readArray(int dim)
    {
        double[] arr = new double[dim];
        for (int i =0;i<dim;i++)
        {
            System.out.print("  v0_"+i+"= ");
            arr[i] = myScan.nextDouble(); 
        }
        return arr;
    }
    // Print für 1D Felder
    public static void print1D(double[] arr){
        System.out.println(Arrays.toString(arr));
    }
    // Berechne die Gravitationskraft
    static double computeGravity (double density_particle, double g, double radius){
        return -4./3 * density_particle * g * Math.PI * Math.pow(radius, 3); // In y-Richtung
    }
    // Berechne die Auftriebskraft
    static double computeBuoyancy (double density_fluid, double g, double radius){
        return 4./3 * density_fluid * g * Math.PI * Math.pow(radius, 3); // In y-Richtung
    }
    // Berechne den Strömungswiderstand
    static double[] computeDrag (double viscosity, double radius, double[] velocity){
        final int dim = velocity.length;
        double[] drag = new double[dim];

        for( int i=0; i<dim; ++i){
            drag[i] = - 6 * Math.PI * viscosity * radius * velocity[i];
        }
        return drag;
    }
    // Lineare kombination von zwei Felder 
    static void add(double[] dst, double[] src, double scalar){
        for( int i=0; i<dst.length; ++i){
            dst[i] += scalar*src[i];
        }
    }
    // Addition von zwei Felder
    static void add(double[] dst, double[] src){
        add(dst, src, 1.0);
    }
    // Berechne die gesamte Kraft
    static double[] computeForce(double density_particle, double g, double radius, double density_fluid, 
                                double viscosity, double[] velocity){

        final int dim = velocity.length;
        double[] force = new double[dim];

        double gravity = computeGravity(density_particle, g, radius);
        double buoyancy = computeBuoyancy(density_fluid, g, radius);
        double[] drag = computeDrag(viscosity, radius, velocity);

        force[1] = gravity + buoyancy;
        // add(force, drag);
        return force;
    }

    public static void main(String[] args) {
        // Konstanten
        final double g = 9.81; // Erdbeschleunigung in m/s^2  

        // Eingaben:
        double radius; // Radius des Partikel, zB. R = 0.1e-3 m; 
        double density_particle; // Dichte des Partikel, zB. rho_p = 7870 Kg/m^3 (Dichte von Eisen)
        double density_fluid; // Dichte des Fluids, zB. rho_f = 1000 Kg/m^3 (Dichte von Wasser)
        double viscosity; //(dynamische) Viskositaet des Fluids, zB. mu = 1e-3 Kg/(m s) (Viskositaet von Wasser)

        // Einlesen der Partikel und Fluiddaten
        System.out.println("Bitte folgende Partikel und Fluiddaten eingeben: ");
        // Scanner myScan = new Scanner(System.in);
        System.out.print("  - Partikelradius(in [m]): ");
        radius = myScan.nextDouble();
        System.out.print("  - Partikeldichte (in [Kg/m^3]): ");
        density_particle = myScan.nextDouble();
        System.out.print("  - Fluiddichte (in [Kg/m^3]): ");
        density_fluid = myScan.nextDouble();
        System.out.print("  - (Dynamische) Fluidviskositaet (in [Kg/(m s)]): ");
        viscosity = myScan.nextDouble();

        //Dimension festlegen:
        System.out.print("  - Dimension d = ");
        int dim = myScan.nextInt();

        // Explizites Eulerverfahren:
        // Einlesen der benötigten Größen
        System.out.println("Anfangsgeschwindigkeit, Zeitschritt und Anzahl an Iterationen eingeben: ");
        System.out.print("  - Anfangsgeschwindigkeit v0 = [");
        double[] v0 = readArray(dim);
        System.out.print("]\n");
        System.out.print("  - Zeitschritt tau = ");
        double tau = myScan.nextDouble();
        System.out.print("  - Anzahl a Iterationen N = ");
        int N = myScan.nextInt();

        // (Anfangs) Geschwindigkeit und Position
        double[] velocity = v0;
        double[] position= new double[dim];
            
        // Hilfsgrößen
        final double mass = 4/3. * Math.PI * Math.pow(radius, 3) * density_particle; // Masse des Partikels
        
    // frame for graphic interface 
    int frameWidth = 1500;
    int frameHeight = 1500;

    JFrame frame = new JFrame("Circling Sphere");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(frameWidth, frameHeight);
    frame.setVisible(true);

    // Instantiate a particle drawing
    ParticlePanel particle = new ParticlePanel();
    frame.add(particle);

        // Zeitschrittverfahren
    for(int it=0; it<N; ++it) 
    {
        // Berechne Gesamtkraft auf Kugel
        double[] force = computeForce(density_particle, g, radius, density_fluid, viscosity, velocity);

        // Explizites Euler-Verfahren
        double[] acceleration = new double[dim]; 
        add(acceleration, force, 1.0/mass);
        add(velocity, acceleration, tau);
        add(position, velocity, tau);
        particle.updateBall(position[0], position[1]);    
        // if ( position[1] < 0 )
        // {
        //     break;
        // }
    }
    System.out.println("-> Berechnete Geschwindigkeit in m/s: ");
    print1D(velocity); 
    System.out.println("-> Berechnete position in m:");
    print1D(position); 

    // Die Referenzloesung 
    double v_ref = 2./9. * (density_particle - density_fluid)/viscosity * g * radius * radius;
    System.out.println("-> Analytische Sinkgeschwindigkeit v_ref = " + v_ref + " m/s");
    } 
}

 class ParticlePanel extends JPanel {
     private double x = 0;
     private double y = 100;
     private double radius = 20;
     private double scale = 2.0;
 
     public void updateBall(double x, double y) {
         this.x = x;
         this.y = y;
 
         try {
             Thread.sleep(2); // Pause for a short time to control the animation speed
         } catch (InterruptedException e) {
             e.printStackTrace();
         }
         repaint(); // Repaint the panel to update the ball
     }
 
     @Override
     protected void paintComponent(Graphics g) {
         super.paintComponent(g);
         Graphics2D g2d = (Graphics2D) g;
 
         // Calculate the center of the frame
         int shift = 100;
         int centerX = getWidth() / 2;
         int centerY = getHeight() /2;
 
         // Translate the coordinate system to center it in the frame
         g2d.translate(centerX - shift , centerY - 2*shift);
 
         // Scale the coordinate system
         g2d.scale(scale, scale);
 
         // Draw the x and y axes
         g2d.setColor(Color.GRAY);
         g2d.drawLine(-1000, 0, 1000, 0); // x-axis
         g2d.drawLine(0, -1000, 0, 1000); // y-axis
 
         // Annotate the coordinate axes
         g2d.setColor(Color.BLACK);
         g2d.drawString("x", 100, 10);
         g2d.drawString("y", -10, -100);
 
         // Draw the falling sphere
         Ellipse2D.Double sphere = new Ellipse2D.Double(x - radius / 2, -y - radius / 2, radius, radius);
         g2d.setColor(Color.BLUE);
         g2d.fill(sphere);
 
         // Central dot
         double radiusCenter = 5.;
         Ellipse2D.Double sphere2 = new Ellipse2D.Double(-radiusCenter / 2, -radiusCenter / 2, radiusCenter, radiusCenter);
         g2d.setColor(Color.RED);
         g2d.fill(sphere2);
     }
 }    
 