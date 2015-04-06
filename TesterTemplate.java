public class Tester {
    public static void main(String[] args) {
        try {
            System.exit(CLASSNAME.test());
        } catch (Throwable e) {
            System.exit(13);
        }
    }
}
