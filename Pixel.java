public class Pixel
{
  private int x;
  private int y;
  private int[] rgb;

  public Pixel(int _x, int _y, int[] _rgb)
  {
    x = _x;
    y = _y;
    rgb = _rgb;
  }

  public void write() {
    System.out.print( "Pixel["+x+"]["+y+"]: ");
    System.out.println(rgb[0]+" "+rgb[1]+" "+rgb[2]);
  }
}
