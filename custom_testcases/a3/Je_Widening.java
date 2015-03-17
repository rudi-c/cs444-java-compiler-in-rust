public class Je_Widening {
    public static int test() {
        byte a = (byte) 2;
        byte b = (byte) 3;
        byte x = a * b; //~ error: cast required for narrowing conversion from `int` to `byte`
        return 0;
    }
}
