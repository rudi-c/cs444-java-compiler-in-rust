public class Je_InstanceOf {
    public Je_InstanceOf() { }
    public static int test() {
        if (new Integer(123) instanceof Character) {
            //~^ error: `instanceof` expression could never be true
            return 100;
        } else {
            return 123;
        }
    }
}
