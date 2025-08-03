import java.util.*;

public class Bild
{
  public static Scanner sc = new Scanner(System.in);
  public int size;
  public Pixel[][] pixels;

  public Bild()
  {
    System.out.println("Geben Sie Bildgroesse an");
    size = sc.nextInt();
    pixels = new Pixel[size][size];

    for( int x = 0; x < size; x++ ) {
      for( int y = 0; y < size; y++ ) {
        System.out.println("Pixel["+x+"]["+y+"]");
        System.out.println("RGB Wert:");
        int[] rgb = new int[3];

        for( int k = 0; k < rgb.length; k++ ) {
          rgb[k] = sc.nextInt();
        }

        pixels[x][y] = new Pixel(x,y,rgb);
      }
    }
  }

  public static void write(Bild bild)
  {
    for( int x = 0; x < bild.size; x++ ) {
      for( int y = 0; y < bild.size; y++ ) {
        bild.pixels[x][y].write();
      }
    }
  }

  public static void main(String[] args)
  {
    Bild beltracchi = new Bild();
    Bild mattise = new Bild();
    write(beltracchi);
    write(mattise);
  }
}
