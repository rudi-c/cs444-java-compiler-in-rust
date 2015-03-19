public class J1_NonConstant {
    public J1_NonConstant() {}
    public static int test() {
        while (false && 1 == J1_NonConstant.test()); // statement is not unreachable
        return 1;
    }
}
