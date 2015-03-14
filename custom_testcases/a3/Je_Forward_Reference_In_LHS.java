public class Je_Forward_Reference_In_LHS {
    public Je_Forward_Reference_In_LHS() {}

    int[] array = new int[123];
    int a = array[b] = 321;
    int b = 123;
}
