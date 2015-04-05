package classes;
public class Super {
    public int a = init("a");
    public Super() {
        System.out.println("Super()");
    }
    public int b = init("b");
    public int init(String s) {
        System.out.println("init("+s+"); a = "+a+", b = "+b);
        return 2;
    }
}
