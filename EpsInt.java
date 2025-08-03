public class EpsInt {
	public static void main (String[] args){
		double x=1.0;
		while((int)(x+1)!=1){
			x/=2.0;
		}
		double eps=2.0*x;
		System.out.println(eps);
	}
}