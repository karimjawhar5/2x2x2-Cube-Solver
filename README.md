# 2x2x2-Cube-Solver
A prolog program using meet-in-middle algorithm to solve 2x2x2 Rubik's cube representation

##introduction
Prolog is a logic programming language associated with artificial intelligence, the goal of this project is to use the meet in the middle alorithm to find ALL of the shortest paths (set of moves) to take a 2x2x2 Rubik's cube from any state (3,674,160 possible states!!) to the solved state! 

##Dependencies
- SWI-Prolog to run prolog scripts: https://www.swi-prolog.org/ (available on Mac OS, and Windows)
- XQuartz to visualize cube representation: https://www.xquartz.org/ (available only for Mac OS)

##Usage
- Download this repository on your desktop or any directory, make sure to change directory accordingly!
### In SWI-Prolog:
-  Run this to change working directory:
```working_directory(CWD,'/Users/<Your Username>/Desktop/2x2x2-Cube-Solver').```
- Run this to use the twosy.pl script:
```[twosy].```
- Run this to see it in action: (you can find more information about the procedures in 2x2x2.pl and twosy.pl)
```state_zero(Solved), shuffle(Solved, Shuffled), twosy(Shuffled, Moves), display_path(Shuffled, Moves)````


##Algorith & Project Details
The meet in the middle algorith is perfect for this use case, since the state space of the 2x2x2 Rubick's cube is huge 3,674,160 possible states to be exact. 

First Let's consider the cube representation included in the 2x2x2.pl and 2x2x2pce.pl, these files create a representation for our 2x2x2 rubik's cube, it keeps track of the cube state, by holding a fixed corner, and in refference of that corner 6 possible moves can be done (bottom, right, left sides. for each side clockwise or counter clockwise 90 degree rotation).

It's been mathematically proven that a 2x2x2 Cube in any state can be solved in 14 moves or less (each move is 90 degree rotation). We take advantage of this fact to in our program.

Here's how it works:
- We apply ONE of EACH possible move to a cube in the solved state, this gives us 6 new states!
- Again we apply ONE of EACH possible move to the given shuffled sube, this also gives us 6 new states!
- On each of those states from the solved and the mixed cubes we do the same, this will grow or spheres exponentially, but remeber we only have to grow each sphere 7 layers out, so 6^7 = 279,936 states.
- because we can guarintee that a 2x2x2 Cube in any state can be solved in 14 moves or less as mentioned above, we know that if we grew both the solved and the mixed cube states 7 layers out each we will end up in a common state.
- after finding that common state we connect the path of the state our original mixed state to our solved state and voila we have a set of moves to solve or cube!
- The reason meet in the middle algorithm so powerful in this case is because generating and storing 279,936x2 is much more efficient that just expanding the solved state to 6^14 = 78,364,164,096 states to fine the solved our mixed state or visa versa, which would practically be impossible to store, search or let alone generate!

##Demo
Youtube Video Demo:
https://youtu.be/ARm3xM-5CZ0

##Attribution
The 2x2x2 Rubik's Cube representation file (`2x2x2.pl`) used in this project was originally created by Christian Schlichtherle and modified by Parke Godfrey.
