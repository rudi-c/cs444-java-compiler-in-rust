public class Je_Return_Void {
    public void a() {
        return;
    }
    public void b() {
        return a(); //~ error: `return` statement with a value in a method returning `void`
    }
}
