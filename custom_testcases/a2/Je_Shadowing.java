public class Je_Shadowing {
    static void test() {
        int a = 0;
        //~^ note: the old definition is here
        int a = 1;
        //~^ error: variable `a` already defined
    }
}
