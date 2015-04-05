public class J1e_PrimArrayCast1 {
    public J1e_PrimArrayCast1() { }
    public static int test() {
        return ((int[])(Object)new short[123])[0];
    }
}
