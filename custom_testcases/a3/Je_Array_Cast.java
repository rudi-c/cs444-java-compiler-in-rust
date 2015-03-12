public class Je_Array_Cast {
    public static int test() {
        Object x = new int[1]; // ok
        Object[] y = new String[2]; // ok
        Cloneable z = new byte[3]; // ok
        String q = new byte[4]; //~ error: cannot convert from array type `byte[]` to `java.lang.String`
        return 0;
    }
}
