public class BinaryTree<V extends Comparable<V>> {
    private Node<V> root;

    public BinaryTree(Node<V> root) {
        this.root = root;
    }

    public Node<V> getRoot() {
        return this.root;
    }

    public void printInorder() {
        printInOrderHelper(root);
    }
    private void printInOrderHelper(Node<V> node) {
        if(node == null){
            return;
        }
        printInOrderHelper(node.getLeft());
        System.out.print(" " + node.getValue());
        printInOrderHelper(node.getRight());
    }

    public void printPreorder(){
        printPreorderHelper(root);
    }
    private void printPreorderHelper(Node<V> node) {
        if(node == null){
            return;
        }
        System.out.print(" " + node.getValue());
        printPreorderHelper(node.getLeft());
        printPreorderHelper(node.getRight());
    }

    public void printPostorder() {
        printPostorderHelper(root);
    }
    private void printPostorderHelper(Node<V> node) {
        if(node == null){
            return;
        }
        printPostorderHelper(node.getLeft());
        printPostorderHelper(node.getRight());
        System.out.print(" " + node.getValue());
    }

    public V[] flatten() {
        int size = getSize(root);
        V[] myList = (V[]) new Comparable[size];
        fillInArray( myList, root, 0);
        sort(myList);
        return myList;
    }
    public int getSize(Node<V> node){
        if(node == null){
            return 0;
        }
            return 1 + getSize(node.getLeft()) + getSize(node.getRight());
    }
    public int fillInArray( V[] arr, Node<V> node, int i){
        if(node == null){
            return i;
        }
        i = fillInArray( arr, node.getLeft(), i);
        arr[i] = node.getValue();
        i = fillInArray(arr, node.getRight(), i + 1);
        return i;
    }

    public void sort(Comparable[] a) {
        int i, j;
        Comparable temp;
        boolean swapped = true;
        for (i = 0; i < a.length && swapped == true; i++) {
            swapped = false;
            for (j = 1; j < a.length - i; j++) {
                if (a[j].compareTo(a[j-1]) < 0) {
                    swapped = true;
                    temp = a[j];
                    a[j] = a[j-1];
                    a[j-1] = temp;
                }
            }
        }
    }

    public void invert() {
        invertHelper(root);
    }

    public Node<V> invertHelper(Node<V> node) {
        if(node == null){
            return null;
        }
        Node <V> dummy = node.getLeft();
        node.setLeft(node.getRight());
        node.setRight(dummy);
        invertHelper(node.getLeft());
        invertHelper(node.getRight());
        return null;
    }

    public boolean containsSubtree(BinaryTree<V> other) {
        if(other == null){
            return true;
        }
        else if(root.getLeft() == null && root.getRight() == null){
            return false;
        }
        V topVal = other.root.getValue();
        V leftVal = other.root.getLeft().getValue();
        V rightVal = other.root.getRight().getValue();
        boolean bool = (containsHelper(topVal, leftVal, rightVal, root.getRight())
                || containsHelper(topVal, leftVal, rightVal, root.getLeft()));
        return bool;
    }
    private boolean containsHelper(V topVal, V leftVal, V rightVal, Node<V> node){
        if(node == null){
            return false;
        }
        if(node.getValue().compareTo(topVal) == 0 && node.getLeft().getValue().compareTo(leftVal) == 0 && node.getRight().getValue().compareTo(rightVal) == 0){
            return true;
        }
        containsHelper(topVal, leftVal, rightVal, node.getLeft());
        containsHelper(topVal, leftVal, rightVal, node.getRight());
        return false;
    }
}
