package a;
public interface Intf1 {
    void derp();
    void herp();
}
package a;
public interface Intf2 {
    void derp();
    void herp(int a);
}
package a;
public interface Intf extends Intf1, Intf2 {
//~^ error: conflicting interface method
}
