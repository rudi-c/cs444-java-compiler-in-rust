public class Je_add_nulls {
    public static int test() {
        int x = null + null; //~ error: expected numeric type, found `null`
        return x;
    }
}
