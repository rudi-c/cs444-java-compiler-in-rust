public class J1e_ArrayWriteEvalOrder {
    public J1e_ArrayWriteEvalOrder() { }
    public static int test() {
        return new J1e_ArrayWriteEvalOrder().go();
    }
    public int go() {
        f()[g()] = f();
        return 0;
    }
    public Object[] f() {
        System.out.println("f");
        return null;
    }
    public int g() {
        System.out.println("g");
        return 123;
    }
}
