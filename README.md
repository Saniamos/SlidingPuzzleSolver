# SlidingPuzzleSolver

Last year we should program a solver for the Sliding Puzzles one had as child in Haskell using Backtracking.  
Since the Puzzles can be solved faster by just using specific steps, I decided to implement that Version instead.  
We were supposed to do this for word puzzels, meaning you give the solver the Puzzle with letters making up one word and the original word and it should arrange the tiles in such a way, that from left to right, top to bottom the word should be readable. So you'll find some code to do just that, along with the possibility to generate a Puzzle from the scrabble dictionary. But since that's rather difficult to read and the words are not that long the more interesting code is where the numbers from 1 to (boardlength^2)-1 are used.  

The implementation is a rather naive one with just a few optimizations. Maybe I'll add some more in time. But that's not going to happen before the word generator is added to the ui and all the documentation is translated to english.  

If you compiled the main.hs you have the following options to call the main function:  

main \<param\> \<filename\>  
where filename is the name of a file, where a Board is defined in this way:  
1,5,2,4,8,3,7,6,  
1,2,3,4,5,6,7,8,  
first is the board as it is to be solved  
second is the board as it should be  

or  
main \<param\> \<x\> \<n\>  
x being the board height and width  
n being the number of steps Haskell is supposed to do random steps in an ordered board to make it unorderd  
  
In both cases param can either be -p meaning you want to play the board yourself using wasd + enter to move the empty field (everything else will ask you if you wan't to quit the game)  
or -m mening you wan't to see a trace of what the solver's steps where  


Of course you can always just load the Haskell and play with it as you wish.  

One last note: the longest a was able to wait for a result and not stoping before it returned was for the input ./Main 40 100000. Whereas .Main 20 100000 solves in about 5min.
