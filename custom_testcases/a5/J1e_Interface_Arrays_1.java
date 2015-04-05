import java.io.*;
public class J1e_Interface_Arrays_1 {
    public J1e_Interface_Arrays_1() { }
    public static int test() {
        Object x = new Serializable[123];
        x = (Cloneable[]) x;
        return 0;
    }
}
