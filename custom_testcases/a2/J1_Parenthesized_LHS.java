public class J1_Parenthesized_LHS {
    public J1_Parenthesized_LHS() { }
    public static int test() {
        int x = 1;
        (x) = (x);
        return x - 1;
    }
}
