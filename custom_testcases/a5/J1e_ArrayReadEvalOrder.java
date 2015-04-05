public class J1e_ArrayReadEvalOrder {
    public J1e_ArrayReadEvalOrder() { }
    public static int test() {
        return new J1e_ArrayReadEvalOrder().go();
    }
    public int go() {
        return f()[g()];
    }
    public int[] f() {
        System.out.println("f");
        return null;
    }
    public int g() {
        System.out.println("g");
        return 123;
    }
}
