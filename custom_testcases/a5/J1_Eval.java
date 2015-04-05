public class J1_Eval {
    public J1_Eval() {}
    public static int test() {
        return new J1_Eval().go();
    }
    public int go() {
        System.out.println(id(1));
        System.out.println(1 != id(2));
        System.out.println(1 < id(2));
        System.out.println(2 <= id(2));
        System.out.println(-123 >= id(-1234));
        System.out.println(3 > -0);
        System.out.println(-123 * id(-123) == 15129);
        System.out.println(33 / id(7));
        System.out.println(33 / id(-7));
        System.out.println(-33 / id(-7));
        System.out.println(false || id("asdf") == "asdf");
        System.out.println((short) 123 == id(123) && (char) 123 == id(123));
        System.out.println((int)id('A'));
        System.out.println((byte) id(123) == (short) 123);
        System.out.println((byte) id(256) == (char) 0);
        System.out.println((short) id(65536) == (byte) 512);
        System.out.println(id(65536) * 32768);
        System.out.println(id(65536) * 65536);
        System.out.println((-1) * id(-2147483648));
        System.out.println(-id(-2147483648));
        System.out.println(id(-2147483648) / -1);
        System.out.println((-123) % id(12));
        System.out.println((-2147483648) % id(-1));
        System.out.println((-3232) % id(-17));
        System.out.println((-3232) % id(17));
        System.out.println((3232) % id(-17));
        System.out.println((3232) % id(17));
        System.out.println((0) % id(-1));
        System.out.println((0) / id(-1));
        System.out.println((0) % id(1));
        System.out.println((0) / id(1));
        System.out.println("AB");
        System.out.println(id("A")+"B");
        System.out.println('A'+id("B"));
        System.out.println("A"+id('B'));
        System.out.println(id("A")+"B"+123);
        System.out.println(id('A')+"B"+(short)id(123));
        System.out.println((char)id(65)+"B");
        System.out.println("AB"+(id('A')+2));
        System.out.println("AB"+id('A')+2);
        System.out.println(""+(byte)id(255));
        System.out.println((int)(char)(short)-id(32768)); // `char` is an unsigned type
        System.out.println((int)(char)(short)id(65535)); // `char` is an unsigned type
        return 0;
    }
    public int id(int a) { return a; }
    public short id(short a) { return a; }
    public char id(char a) { return a; }
    public byte id(byte a) { return a; }
    public boolean id(boolean a) { return a; }
    public String id(String a) { return a; }
}

