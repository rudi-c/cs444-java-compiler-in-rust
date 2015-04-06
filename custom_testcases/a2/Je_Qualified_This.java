public class Je_Qualified_This {
    public Je_Qualified_This() { }
    public void go() {
        int x = Object.this.x; //~ error: invalid qualified `this`
    }
    public static int test() {
        return Je_Qualified_This.this.y; //~ error: cannot use `this` in static context
    }
}
