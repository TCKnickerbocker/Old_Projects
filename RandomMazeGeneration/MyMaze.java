// Names: Thomas Knickerbocker

import java.util.Random;

public class MyMaze{
    private Cell[][] cellMaze;
    private int startRow;
    private int endRow;
    private int rows;
    private int cols;
    private int myRow;
    private int myCol;

    public MyMaze(int rows, int cols, int startRow, int endRow) {
        this.startRow = startRow;
        this.endRow = endRow;
        cellMaze = new Cell[rows][cols];
        this.rows = rows;
        this.cols = cols;
        myRow = startRow;
        myCol = 0;
        for(int i = 0; i < rows; i++){
            for(int j = 0; j < cols; j++){
                cellMaze[i][j] = new Cell();
            }
        }
        //initializes the cellMaze, start & endrow, myRows & cols, and the total length of array
    }

    public static MyMaze makeMaze(int rows, int cols, int startRow, int endRow) {
        Stack1Gen <int[]> myStack = new Stack1Gen();
        MyMaze maze = new MyMaze(rows, cols, startRow, endRow);
        maze.cellMaze[startRow][0].setVisited(true);
        int[] start = {startRow, 0};
        myStack.push(start);
        //Initializes maze, stack, and start (the vehicle for stack pushes

        while (myStack.isEmpty() == false) {
            int[] top = myStack.top(); //cords of previous cell added
            int[] newTop = new int[2]; //cords of cell about to be added
            int openNeighbors = 0;
            boolean neighbors = false;
            boolean left = false;
            boolean right = false;
            boolean up = false;
            boolean down = false;
            String used = "none";
            maze.myRow = top[0];
            maze.myCol = top[1];
            maze.cellMaze[top[0]][top[1]].setVisited(true);
            //

            //checks neighbors and updates available directions, if any
            if (maze.myRow + 1 < maze.cellMaze.length && maze.cellMaze[maze.myRow + 1][maze.myCol].getVisited() == false) { //down
                openNeighbors++;
                down = true;
            }
            if (maze.myCol + 1 < maze.cellMaze[0].length && maze.cellMaze[maze.myRow][maze.myCol + 1].getVisited() == false) { //right
                openNeighbors++;
                right = true;
            }
            if (maze.myRow - 1 >= 0 && maze.cellMaze[maze.myRow - 1][maze.myCol].getVisited() == false) { //up
                openNeighbors++;
                up = true;
            }
            if (maze.myCol - 1 >= 0 && maze.cellMaze[maze.myRow][maze.myCol - 1].getVisited() == false) { //left
                openNeighbors++;
                left = true;
            }
            if (openNeighbors == 0) {
                myStack.pop();
                continue;
            }
            //finds a random neighbor if there are any nearby, then pushes neighbor to stack, updates isvisited,
            // and updates appropriate row/col

            //instead of having things done by cases, ill have to know how many neighbors are open, pick between down and right
            if (openNeighbors > 0) {
                while (neighbors == false) {
                    int random = (int) (Math.random() * 4) + 1; //random #1-4
                    switch (random) {
                        case 1: //right
                            if (right == true) {
                                maze.myCol++;
                                used = "right";
                                newTop[0] = maze.myRow;
                                newTop[1] = maze.myCol;
                                maze.cellMaze[maze.myRow][maze.myCol].setVisited(true);
                                myStack.push(newTop);
                                neighbors = true;
                            }
                            break;
                        case 2: //left
                            if (left == true) {
                                maze.myCol--;
                                used = "left";
                                newTop[0] = maze.myRow;
                                newTop[1] = maze.myCol;
                                maze.cellMaze[maze.myRow][maze.myCol].setVisited(true);
                                myStack.push(newTop);
                                neighbors = true;
                            }

                            break;
                        case 3: //up
                            if (up == true) {
                                maze.myRow--;
                                used = "up";
                                newTop[0] = maze.myRow;
                                newTop[1] = maze.myCol;
                                maze.cellMaze[maze.myRow][maze.myCol].setVisited(true);
                                myStack.push(newTop);
                                neighbors = true;
                            }
                            break;
                        case 4: //down
                            if (down == true) {
                                maze.myRow++;
                                used = "down";
                                newTop[0] = maze.myRow;
                                newTop[1] = maze.myCol;
                                maze.cellMaze[maze.myRow][maze.myCol].setVisited(true);
                                myStack.push(newTop);
                                neighbors = true;
                            }
                            break;
                    }
                }
            } //end find neighbor
            //Wall removal:
            switch (used) {
                case "right": //previous cell must open at right
                    if(top[1] != cols-1) { //if not last column
                        maze.cellMaze[top[0]][top[1]].setRight(false);
                    }
                    break;
                case "left": //new cell must be open right
                    if(newTop[1] != cols-1) { //if not last column
                        maze.cellMaze[newTop[0]][newTop[1]].setRight(false);
                    }
                    break;
                case "up": //new cell must be open down
                    if(newTop[0] != rows-1) { //if not bottom row
                        maze.cellMaze[newTop[0]][newTop[1]].setBottom(false);
                    }
                    break;
                case "down": //previous cell open down
                    if(top[0] != rows-1) { //if not bottom row
                        maze.cellMaze[top[0]][top[1]].setBottom(false);
                    }
                    break;
                case "none":
                    break;
            }
        } //end of while loop
        maze.cellMaze[endRow][cols-1].setRight(false); //set end to visited
        return maze;
    }

    public void printMaze(boolean path) {
        String vertical = "|";
        String horizontal = "|---|";
        int lastJ = 0;
        for (int j = 0; j < this.cols; j++) { //makes top wall
            System.out.print(horizontal);
        }
        System.out.println();

        //actual maze begins
        for (int i = 0; i < this.rows; i++) {
            if (i != startRow) {
                System.out.print(vertical);
            } else {
                System.out.print(" ");
            } //left boundary/maze entrance done
            for (int j = 0; j < cols; j++) {
                if (this.cellMaze[i][j].getVisited() == true) {
                    System.out.print(" * ");
                } else { //visit check
                    System.out.print("   ");
                }
                if (this.cellMaze[i][j].getRight() == true && j != cols - 1) { //because we already print the last line
                    System.out.print(vertical);
                    System.out.print(vertical);
                } else if(j != cols-1) { //right borders
                    System.out.print("  ");
                }
                lastJ = j;
            } //end of inner loop
            if(cellMaze[i][lastJ].getRight() == true) { //far right vertical bar
                System.out.print(vertical);
            }
            System.out.println();
            for(int k = 0; k < this.cols; k++) { //bottom bars/inbetween "cells"
                if ((this.cellMaze[i][k].getBottom() == true)) {
                    System.out.print(horizontal);
                }
                else{
                    System.out.print("|   |");
                }
            }
            System.out.println();
        } //end of outer loop
    }

    // Breadth-first search to solve maze
    public void solveMaze() {
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < this.cols; j++) {
                this.cellMaze[i][j].setVisited(false);
            }
        }  //sets everything to unvisited, creates queue at startRow, and sets myRow & myCol
        Q2Gen<int[]> q = new Q2Gen();
        int[] top = {this.startRow, 0};
        q.add(top);

        while (q.length() != 0) {
            this.myRow = top[0];
            this.myCol = top[1];
            this.cellMaze[top[0]][top[1]].setVisited(true);
            if(myRow == endRow && myCol == cols-1){ //endcheck
                return;
            }
            // Enqueue all unvisited neighbors
            if (top[0] - 1 >= 0 && this.cellMaze[top[0] - 1][top[1]].getVisited() == false && this.cellMaze[top[0]-1][top[1]].getBottom() == false) { //up
                int[] x = {top[0]-1, top[1]};
                q.add(x);
            }
            if (top[1] - 1 >= 0 && this.cellMaze[top[0]][top[1] - 1].getVisited() == false && this.cellMaze[top[0]][top[1]-1].getRight() == false) {//left
                int[] x = {top[0], top[1]-1};
                q.add(x);
            }
            if (top[0]+1 < this.rows && this.cellMaze[top[0] + 1][top[1]].getVisited() == false && this.cellMaze[top[0]][top[1]].getBottom() == false) { //down
                int[] x = {top[0]+1, top[1]};
                q.add(x);
            }
            if (top[1]+1 < this.cols && this.cellMaze[top[0]][top[1] + 1].getVisited() == false && this.cellMaze[top[0]][top[1]].getRight() == false) { //right
                int[] x = {top[0], top[1]+1};
                q.add(x);
            }
            top = q.remove(); //remove and save last iteration's item
        }
    }

    public static void main(String[] args){
        /* Any testing can be put in this main function */
        MyMaze a = makeMaze(10, 10, 1, 8);
        a.solveMaze();
        a.printMaze(true);
    }
}
