/**
 * Parser/weeder:
 * - Shouldn't be able to declare variables of type null.
 */
public class Je_NullType {

    public Je_NullType() {
        null x = 123;
    }
}
