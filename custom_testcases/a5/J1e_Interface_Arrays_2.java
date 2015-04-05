import java.io.*;
public class J1e_Interface_Arrays_2 {
    public J1e_Interface_Arrays_2() { }
    public static int test() {
        Object x = new Serializable[123];
        x = (int[]) x;
        return 0;
    }
}
