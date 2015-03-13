package A;
public class Base {
    protected void hello() { }
}
package A;
public class Child1 extends Base {
    // inherit `hello`
}
package A;
public class Child2 extends Base {
    protected void call_hello(Child1 c) {
        c.hello();
    }
}
