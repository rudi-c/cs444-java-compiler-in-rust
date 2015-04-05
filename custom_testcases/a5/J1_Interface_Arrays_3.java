import java.io.*;
public class J1_Interface_Arrays_3 {
    public J1_Interface_Arrays_3() { }
    public static int test() {
        Object x = new Serializable[123];
        x = (Object[]) x;
        return 0;
    }
}
