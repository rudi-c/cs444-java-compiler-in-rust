import java.io.*;
public class J1_Interface_Arrays_4 {
    public J1_Interface_Arrays_4() { }
    public static int test() {
        Serializable[] array = new Serializable[10];
        array[0] = new int[12];
        ((int[])array[0])[3] = 6969;
        array[1] = new char[10];
        array[2] = array;
        Object[] sup = array;
        System.out.println(sup instanceof Serializable);
        System.out.println(sup instanceof Serializable[]);
        System.out.println(sup[0] instanceof int[]);
        System.out.println(sup[0] instanceof char[]);
        System.out.println(sup[0] instanceof Serializable);
        System.out.println(sup[0] instanceof Serializable[]);
        System.out.println(sup[1] instanceof int[]);
        System.out.println(sup[1] instanceof char[]);
        System.out.println(sup[1] instanceof Serializable);
        System.out.println(sup[1] instanceof Serializable[]);
        System.out.println(sup[2] instanceof int[]);
        System.out.println(sup[2] instanceof char[]);
        System.out.println(sup[2] instanceof Serializable);
        System.out.println(sup[2] instanceof Serializable[]);
        Object obj = sup;
        Cloneable sub = (Cloneable) obj;
        Serializable ser = (Serializable) sup;
        array = (Serializable[]) ser;
        System.out.println(((int[])array[0])[3]);
        return 0;
    }
}
