package A;
public class Test {
    static void test() {
        B.Derp x = null;
        //~^ `A.B` is a type, not a package
    }
}

package A;
public class B {
    public B() { }
}

package B;
public class Derp {
    public Derp() { }
}
