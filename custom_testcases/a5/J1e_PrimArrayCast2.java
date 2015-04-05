public class J1e_PrimArrayCast2 {
    public J1e_PrimArrayCast2() { }
    public static int test() {
        return ((int[])(java.io.Serializable)new Object[123])[0];
    }
}
