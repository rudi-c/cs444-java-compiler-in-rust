public class Je_Array_Explicit_Cast {
    public static int test() {
        int[] x = (int[]) new byte[123]; //~ error: invalid cast from `byte[]` to `int[]`
    }
}
