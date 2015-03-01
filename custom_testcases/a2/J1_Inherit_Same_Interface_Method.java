package a;
public interface Intf1 {
    void derp();
}
package a;
public interface Intf2 extends Intf1 { }
package a;
public interface Intf3 extends Intf1 { }
package a;
public interface Intf4 extends Intf2, Intf3 { }
package a;
public interface Intf5 extends Intf1, Intf2, Intf3, Intf4 { }
package a;
public abstract class Abstract implements Intf2, Intf4 { }
package a;
public class Concrete implements Intf2, Intf4 {
    public void derp() { }
}
