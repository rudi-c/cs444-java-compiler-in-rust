public class J1e_ArrayDynamicType {
    public J1e_ArrayDynamicType() { }
    public static int test() {
        Integer[] array = new Integer[10];
        Number[] derp = array;
        derp[3] = new Short((short) 324);
        return 0;
    }
}

