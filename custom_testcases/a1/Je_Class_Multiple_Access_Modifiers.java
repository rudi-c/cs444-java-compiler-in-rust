/**
 * Parser/weeder:
 * - Shouldn't have both private and public as modifiers.
 */
protected class Je_Class_Multiple_Access_Modifiers {
    public protected int x = 0;
    public Je_Class_Multiple_Access_Modifiers() {}
}
