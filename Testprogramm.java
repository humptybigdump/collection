class Testprogramm {

    public static void main(String[] args) {
        byte a = 1;
        byte b = 3;
        byte c = 96;
        
        for (byte m = -128; m < 127; m++) {
            if ((byte)(m * a) == (byte)1) {
                System.out.println("[a] = [" + a + "] hat das Inverse [" + m + "]");
            }
            if ((byte)(m * b) == (byte)1) {
                System.out.println("[b] = [" + b + "] hat das Inverse [" + m + "]");
            }
            if ((byte)(m * c) == (byte)1) {
                System.out.println("[c] = [" + c + "] hat das Inverse [" + m + "]");
            }
        }
        
    }

}
