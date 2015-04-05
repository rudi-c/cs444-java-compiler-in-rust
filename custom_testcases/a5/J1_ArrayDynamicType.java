public class J1_ArrayDynamicType {
    public J1_ArrayDynamicType() { }
    public static int test() {
        Integer[] array = new Integer[10];
        array[0] = new Integer(123);
        array[1] = new Integer(234);
        Number[] derp = array;
        Object[] herp = array;
        if (derp != herp) return 1;
        if (derp[1].intValue() != 234) return 1;
        if (derp[2] != null) return 1;
        if (!herp[0].toString().equals((Object) "123")) return 1;
        herp[3] = new Integer(3);
        derp[4] = new Integer(4);
        if (!(derp[3] instanceof Number)) return 1;
        if (!(herp[4] instanceof Integer)) return 1;

        Number[] num_array = new Number[10];
        num_array[0] = new Integer(123);
        ((Object[]) num_array)[2] = new Short((short) 321);
        if (num_array[2].intValue() != 321) return 1;
        return 0;
    }
}
