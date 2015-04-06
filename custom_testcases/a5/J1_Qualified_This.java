public class J1_Qualified_This {
    int x;
    public J1_Qualified_This() { }
    public int go() {
        int y = J1_Qualified_This.this.x;
        J1_Qualified_This.this.x = y * 10;
        return y;
    }
    public static int test() {
        J1_Qualified_This t = new J1_Qualified_This();
        t.x = 6;
        return t.go() + t.x;
    }
}
