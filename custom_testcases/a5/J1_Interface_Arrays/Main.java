public class Main {
    public Main() { }

    public static int test() {
        intfs.A[] arr = new intfs.A[2];
        arr[0] = new classes.A();
        arr[1] = new classes.AB();
        intfs.Aint[] arr2 = new intfs.Aint[3];
        arr2[0] = new classes.Aint();
        arr2[2] = new classes.AintB();
        intfs.B[] arr3 = new intfs.B[3];
        arr3[1] = new classes.AB();
        arr3[2] = new classes.AintB();
        intfs.AB[] arr4 = new intfs.AB[4];
        arr4[0] = new classes.AB();
        intfs.AintB[] arr5 = new intfs.AintB[5];
        arr5[0] = new classes.AintB();
        System.out.println(arr instanceof intfs.A[]);
        System.out.println(arr instanceof intfs.B[]);
        System.out.println(arr instanceof intfs.AB[]);
        System.out.println(arr2 instanceof intfs.Aint[]);
        System.out.println(arr2 instanceof intfs.B[]);
        System.out.println(arr2 instanceof intfs.AintB[]);
        System.out.println(arr3 instanceof intfs.A[]);
        System.out.println(arr3 instanceof intfs.Aint[]);
        System.out.println(arr3 instanceof intfs.B[]);
        System.out.println(arr3 instanceof intfs.AintB[]);
        System.out.println(arr3 instanceof intfs.AB[]);
        System.out.println(arr4 instanceof intfs.A[]);
        System.out.println(arr4 instanceof intfs.B[]);
        System.out.println(arr4 instanceof intfs.AB[]);
        System.out.println(arr5 instanceof intfs.Aint[]);
        System.out.println(arr5 instanceof intfs.B[]);
        System.out.println(arr5 instanceof intfs.AintB[]);
        return ((intfs.B[])arr3).length + ((intfs.B[])arr4).length + ((intfs.Aint[])arr5)[0].a();
    }
}
