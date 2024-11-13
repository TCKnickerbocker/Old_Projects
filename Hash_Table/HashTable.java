import java.util.Scanner;
import java.io.*;

public class HashTable<T> {
    private NGen<T>[] arr;
    private String hashType;
    public HashTable() { //initial length is 101
        arr = new NGen[149];
    }

    public int Hash1(T key) { //obtains hash spots for table by adding up first three digits
        String token = key.toString();
        int hash = 0;
        for (int i = 0; i < token.length(); i++) {
                hash += token.charAt(i);
            if(i == 6){
                hash += token.charAt(i);
            }
            if(i == 3){
                hash += token.charAt(i);
            }
            else{
                hash += 1;
            }
           if(i == 8){
               hash -= token.charAt(i);
           }
        }
        return (hash * 7) % arr.length;
    }

    public int Hash2(T key) { //obtains hash spot by adding first and last character of string then using modulo
        String token = key.toString();
        int hash = 0;
        hash += token.charAt(0);
        hash *= token.charAt(token.length()-1);
        if(token.length() > 1){
            hash += token.charAt(1);
        }
        if(token.length() > 5){
            hash += token.charAt(4);
        }
        if(token.length() > 8){
            hash *= 1.2;
        }

        return (7 * hash) % arr.length;
    }

    public int Hash3(T key) {
        String token = key.toString();
        int hash = 0;
        for (int i = 0; i < token.length(); i++) {
                hash += token.charAt(i);
                if(i == 2){
                    hash += token.charAt(i);
                }
                if(i == 4){
                    hash = hash + 3;
                }
                if(i == 1){
                    hash += 2 * token.charAt(i);
                }
                if(i >= 7){
                    hash += token.charAt(i);
                }
        }
        return (7 * hash) % arr.length;
    }

    public void add(T data) {
        if (data == null) {
            return;
        }
        int hNum; //calls to hash below
        if(this.hashType.equals("hash1")){
            hNum = Hash1(data);
        }
        else if(this.hashType.equals("hash2")){
            hNum = Hash2(data);
        }
        else {
            hNum = Hash3(data);
        }
        NGen<T> myList = arr[hNum];
        NGen<T> newNode = new NGen<T>(data, null);
        if (myList == null) { //empty chain case
            arr[hNum] = newNode;
        }
        else { //non-empty chain case
            while (myList.getNext() != null) {
                if(data.equals(myList.getData())){
                    return;
                }
                myList = myList.getNext();
            }
            myList.setNext(newNode);
        }
    }

    public void display() {
        int openSpots = 0;
        int longest = 0;
        int tempLongest = 0;
        int count = 0; //count is the # of unique tokens
        int nonEmpty = 0;
        int num = 0;
        NGen<T> ptr; //ptr is used to traverse the chain occupying our spot

        for (int i=0; i<arr.length; i++) {
            ptr = arr[i];
            tempLongest = 0;
            num = 0;
            if (ptr != null) { //if there's something in this hash spot
                nonEmpty++;
                num++;
                count++;
                tempLongest++;
                if (tempLongest > longest){
                    longest = tempLongest;
                }
                while (ptr.getNext() != null) { //traverses chain, finds longest, and counts unique tokens
                    count++;
                    num++;
                    tempLongest++;
                    if (tempLongest > longest){
                        longest = tempLongest;
                    }
                    ptr = ptr.getNext();
                }
                System.out.printf(i + ": " + num);
            }
            else {
                openSpots++;
                System.out.printf(i + ": 0");
            }
            System.out.println();
        }
        System.out.println("average collision length: " + count/nonEmpty);
        System.out.println("longest chain: " + longest);
        System.out.println("unique tokens: " + count);
        System.out.println("empty indixies: " + openSpots);
        System.out.println("non-empty indicies: " + nonEmpty);
    }

    public static void main(String[] args) {

        HashTable myTable = new HashTable();
        String userString = ""; //used to select a hash function #
        Scanner s = new Scanner(System.in);
        while(userString.equals("hash1") != true && userString.equals("hash2")!= true &&
                userString.equals("hash3") != true){
            System.out.println("Enter a hash function to use (hash1, hash2, or hash3)");
            userString = s.nextLine();
        }
        myTable.hashType = userString;
        Scanner readFile = null;
        int count = 0;
        String str;

        String fileName = ""; //below is mostly an adaptation of textscan
        Scanner input = new Scanner(System.in);
        System.out.println("Enter the name of the file you'd like to read: ");
        fileName = input.nextLine();
        System.out.println();
        System.out.println("Attempting to read from file: " + fileName);
        try {
            readFile = new Scanner(new File(fileName));
        }
        catch (FileNotFoundException e) {
            System.out.println("File: " + fileName + " not found");
            System.exit(1);
        }
        System.out.println("Connection to file: " + fileName + " successful");
        System.out.println();
        if(fileName.equals("keywords.txt")){ //special case
            NGen [] newArr = new NGen [313];
            myTable.arr =  newArr;
        }
        while (readFile.hasNext()) { //adds all the lines from the file
            str = readFile.next();
            System.out.println("Token found: " + str);
            count++;
            myTable.add(str);
        }
        System.out.println();
        System.out.println(count + " Tokens found");
        System.out.println();

        //myTable.pwint();
        myTable.display();
    }
}

