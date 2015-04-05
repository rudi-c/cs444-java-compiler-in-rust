public class J1_ToString_Null {
    public J1_ToString_Null() { }
    public String toString() { return null; }
    public static int test() {
        J1_ToString_Null x = new J1_ToString_Null();
        J1_ToString_Null y = null;
        System.out.println(x + " " + null);
        System.out.println(y + " " + x);
        return 0;
    }
}
