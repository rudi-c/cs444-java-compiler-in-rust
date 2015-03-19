public class Je_NonConstant {
    public Je_NonConstant() {}
    public static int test() {
        while (true || 1 == Je_NonConstant.test());
    } //~error: missing return statement
}
