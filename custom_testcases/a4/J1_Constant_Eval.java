public class J1_Constant_Eval {
    public J1_Constant_Eval() {}
    public static int test() {
        if (true) while (1 == 1);
        else if (true) while (1 != 2);
        else if (true) while (1 < 2);
        else if (true) while (2 <= 2);
        else if (true) while (-123 >= -1234 && 3 > -0);
        else if (true) while (-123 * -123 == 15129);
        else if (true) while (33 / 7 == 4 && 33 / -7 == -4 && -33 / -7 == 4);
        else if (true) while (false || "asdf" == "asdf");
        else if (true) while ((short) 123 == 123 && (char) 123 == 123);
        else if (true) while (65 == 'A');
        else if (true) while ((byte) 123 == (short) 123);
        else if (true) while ((byte) 256 == (char) 0);
        else if (true) while ((short) 65536 == (byte) 512);
        else if (true) while (65536 * 32768 == -2147483648);
        else if (true) while (65536 * 65536 == 0);
        else if (true) while ((-1) * -2147483648 == -2147483648 && -(-2147483648) == -2147483648 && (-2147483648) / -1 == -2147483648);
        else if (true) while ((-123) % 12 == -3);
        else if (true) while ((-2147483648) % -1 == 0);
        else if (true) while ("AB" == "AB");
        else if (true) while ("AB" == "A"+"B");
        else if (true) while ("AB" == 'A'+"B");
        else if (true) while ("AB" == "A"+'B');
        else if (true) while ("AB123" == "A"+"B"+123 && "AB123" == 'A'+"B"+(short)123 && "AB" == (char)65+"B");
        else if (true) while ("AB67" == "AB"+('A'+2) && "ABA2" == "AB"+'A'+2);
        else if (true) while ("-1" == ""+(byte)255);
        else if (true) while ("32768" == ""+(int)(char)(short)-32768); // `char` is an unsigned type
        else while (true);
    }
}
