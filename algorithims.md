# Possible Algorithms
 - Minimax
 - Minimax w/ Alpha Beta pruning
   - Move ordering greatly effects this.
 - MTD-f
   - Performs a series of zero-window a/b calls while narrowing the upper/lower bound based on the results
   - Requires a transposition table to be efficient because of the repeated searches.
 - NegaScout (Principal Variation Search)
   - Performs zero-width searches on non-pvs nodes. 
   - The first node is considered the pvs node (meaning this should be the best move) and is searched fully. The remaining child nodes
     are searched with zero-width searches.
 - MTD-f + iterative deepening
   - Iterative deepening can be used so that the search can exit early if necessary. 
   - It also allows for better move ordering by filling up the transposition table with earlier, smaller searches.
 - SSS*
   - Best first algorithm that is similary to A* and trades space for speed.
 
# General Improvements
 - Transposition table
   - Stores previous results of a search in a table. This has made a major impact on performance. 
   - Opens up the possibility of storing more info from previous searches like implementing the "Killer Hueristic"
 - Move ordering
   - Searching the best moves first could greatly improve performance especially in the a/b algorithms. 
   - The "Killer Heuristic" stores the node that previously caused a cutoff at the depth. We can search that node first 
     to try to cause the cutoff again.
   - For games like tictactoe, we could order moves based on the number of possible wins at a certain point. 
     - For TicTacToe this would mean the center moves are optimal.
     - Games like chess wouldn't be as easy.
 - Parallelization
   
   
