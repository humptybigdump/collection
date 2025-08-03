// Dieser Code hält sich nicht an die Bewertungsrichtlinien!

public class Vector {
    private double x;
    private double y;
    private double z;

    // Konstruktor, der die Attribute setzt
    public Vector(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    // Konstruktor, der einen Nullvektor erstellt
    public Vector() {
        this(0.0, 0.0, 0.0);
    }

    // Methode zur Berechnung der Länge des Vektors
    public double length() {
        return Math.sqrt(x * x + y * y + z * z);
    }

    // Methode zur Addition von zwei Vektoren
    public Vector add(Vector other) {
        return new Vector(this.x + other.x, this.y + other.y, this.z + other.z);
    }
}
