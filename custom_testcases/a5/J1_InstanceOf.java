import java.io.*;

public class J1_InstanceOf {
    public J1_InstanceOf() {}
    public static int test() {
        Object int_array = new int[10];
        Serializable char_array = new char[10];
        Object bool_array = new boolean[10];
        Cloneable short_array = new short[10];
        Object byte_array = new byte[10];
        J1_InstanceOf[] derp_array = new J1_InstanceOf[10];
        Object as_object = derp_array;

        System.out.println("int[] instanceof int[]");
        if (!(int_array instanceof int[])) System.out.println("FAIL");

        System.out.println("int[] instanceof char[]");
        if (int_array instanceof char[]) System.out.println("FAIL");
System.out.println("int[] instanceof Object[]");
        if (int_array instanceof Object[]) System.out.println("FAIL");

        System.out.println("int[] instanceof Object");
        if (!(int_array instanceof Object)) System.out.println("FAIL");

        System.out.println("int[] instanceof Serializable");
        if (!(int_array instanceof Serializable)) System.out.println("FAIL");

        System.out.println("int[] instanceof Cloneable");
        if (!(int_array instanceof Cloneable)) System.out.println("FAIL");

        System.out.println("int[] instanceof J1_InstanceOf");
        if (int_array instanceof J1_InstanceOf) System.out.println("FAIL");

        System.out.println("char[] instanceof char[]");
        if (!(char_array instanceof char[])) System.out.println("FAIL");

        System.out.println("char[] instanceof short[]");
        if (char_array instanceof short[]) System.out.println("FAIL");

        System.out.println("boolean[] instanceof boolean[]");
        if (!(bool_array instanceof boolean[])) System.out.println("FAIL");

        System.out.println("short[] instanceof short[]");
        if (!(short_array instanceof short[])) System.out.println("FAIL");

        System.out.println("byte[] instanceof byte[]");
        if (!(byte_array instanceof byte[])) System.out.println("FAIL");

        System.out.println("byte[] instanceof Cloneable[]");
        if (byte_array instanceof Cloneable[]) System.out.println("FAIL");

        System.out.println("J1_InstanceOf[] instanceof Object[]");
        if (!(as_object instanceof Object[])) System.out.println("FAIL");

        System.out.println("J1_InstanceOf[] instanceof Object");
        if (!(derp_array instanceof Object)) System.out.println("FAIL");

        System.out.println("J1_InstanceOf[] instanceof Cloneable[]");
        if (derp_array instanceof Cloneable[]) System.out.println("FAIL");

        return 0;
    }
}
