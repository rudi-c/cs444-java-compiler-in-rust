public class J1_NoShadowing {
    static void test() {
        {
            int a = 0;
        }
        {
            int a = 1;
            if (a == 1) {
                int b = 2;
            } else {
                int b = 3;
            }
        }
    }
}
