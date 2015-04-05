package classes;
public class Sub extends Super {
    public int c = init("c");
    public Sub() {
        System.out.println("Sub()");
    }
    public int d = init("d");
    public int init(String s) {
        System.out.println("init("+s+"); a = "+a+", b = "+b+", c = "+c+", d = "+d);
        return 4;
    }
}

