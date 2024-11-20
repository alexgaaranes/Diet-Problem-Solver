# Name: Alexander Gabriel A. Aranes
# Section: AB4L
# Stud no.: 2023-65885
# Code Desc:    This script contains a function that calculates the optimal value from tableau
#               using Simplex Method. To make the code shorter and faster, the author used
#               different functions from R. One notable function used is unique() which helped
#               in getting basic solutions in O(n) time. Getting the sum and compare it to 1 can
#               be another way but the values might coincidentally sum up to 1 by having more than
#               one non-zero function.

# Simplex Method Function
Simplex <- function(tableau, isMax){
  r <- nrow(tableau) # Get dimensions
  c <- ncol(tableau)
  
  while(min(tableau[r,])<0){ # Proceed to another iteration if there's a negative value at A[r,]
    PC <- order(tableau[r,])[1] # Get the column index of the highest negative magnitude
    
    # Get the row index of smallest positive test ratio
    testRatios <- tableau[1:(r-1),c]/tableau[1:(r-1),PC]
    PR <- NA
    for(rowInd in order(testRatios)){
      if(testRatios[rowInd] > 0){
        PR <- rowInd
        break;
      }
    }
    print(tableau)
    # Normalize the Pivot Row
    tableau[PR,] <- tableau[PR,]/tableau[PR,PC]
    for(k in 1:r){  # Eliminate A[row,PC] (skip PR)
      if(k!=PR) tableau[k,] <- tableau[k,] - tableau[PR,]*tableau[k,PC]
    }
  
    # Get the Basic Solution
    if(!(isMax || (min(tableau[r,])<0))){  # If isMax==F and no more negative at bottom row
      finalSol <- tableau[r,1:(c-1)]
      finalSol[c-1] <- tableau[r,c]
      return(list(finaltableau=tableau, basicSolution=matrix(
        finalSol, nrow=1), Z=tableau[r,c]))
    }
    
    # Maximum format of Basic Solution
    finalSol <- matrix(ncol=c-1)
    for(col in 1:(c-1)){
      targetCol <- tableau[,col]
      # Check if there's only one non-zero in the column
      if(length(unique(targetCol))==2 && tableau[order(targetCol)[1],col]==0){
        finalSol[1,col] <- tableau[order(targetCol)[length(targetCol)],c]  # Get the corresponding solution
      } else {
        finalSol[1,col] <- 0  # Value is zero if more than one is non-zero in the col
      }
    }
    print(finalSol)
  }
  # Only executes if isMax == T
  return(list(finalTableau=tableau, basicSolution=finalSol, Z=finalSol[1,ncol(finalSol)]))
}

# TEST CASE (Uncomment to run)
# TestMaxTableau = matrix(c(6,14,1,0,0,0,800,1000,2000,0,1,0,0,120000,1,1,0,0,1,0,95,-4000,-5000,0,0,0,1,0), ncol = 7, byrow = T)
# print(Simplex(TestMaxTableau, T))  # Maximize
# 
# TestMinTableau = matrix(c(1,7,1,0,0,14,2,6,0,1,0,20,-4,-20,0,0,1,0), byrow=T, nrow=3)
# print(Simplex(TestMinTableau, F))


