public class J1_Cast_All_The_Way_Down {
    public static int test(int x) {
        return (int)((byte)((short)x) + (byte)((char)x));
    }
}
