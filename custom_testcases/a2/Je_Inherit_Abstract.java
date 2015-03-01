package a;
public interface Intf {
    void herp();
}
package a;
public class Class implements Intf {
//~^ error: class with abstract methods must be abstract
}
