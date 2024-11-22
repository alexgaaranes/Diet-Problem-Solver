# Functions 

# TEST RUN
testRun <- function(indices){
  len <- length(indices)
  # Construct the Tableau based on the selected food
  tableau <- matrix(nrow=len+1,ncol=0)
  acm <- matrix(ncol=len+1,nrow=0)
  for(cat in 4:14){ # Nutrition Constraints
    eqMax <- c()
    eqMin <- c()
    for(i in indices){  # Append the x_i val
      eqMax <- append(eqMax, -nutritionTable[[cat]][i])
      eqMin <- append(eqMin, nutritionTable[[cat]][i])
    }
    acm <- rbind(acm,c(eqMax,-nutritionReq$Maximum[cat-3]))
    acm <- rbind(acm,c(eqMin,nutritionReq$Minimum[cat-3]))
  }
  obj <- c(nutritionTable$Price_Serving[indices],1)  # get the obj function
  
  for(i in 1:len){ # Serving Constraints
    eqServ <- c()
    for(j in 1:len){
      if(j==i) eqServ <- append(eqServ,1)
      else eqServ <- append(eqServ,0)
    }
    acm <- rbind(acm, c(-eqServ,-10))
    acm <- rbind(acm,c(eqServ,0))
  }
  # Transpose to prepare for Minimization
  acm <- t(acm)
  for(i in 1:(ncol(acm))){
    tableau <- cbind(tableau,c(acm[,i]))  
  }
  tableau[nrow(tableau),] <- -tableau[nrow(tableau),]
  # Attach Slacks
  for(i in 1:(len+1)){
      slk <- c()
      for(j in 1:(len+1)){
        if(i == j) slk <- append(slk, 1)
        else slk <- append(slk, 0)
      }
      tableau <- cbind(tableau,slk)
  }
  obj[length(obj)] <- 0
  tableau <- cbind(tableau,obj) # attach obj function
  
  return(list(A=tableau,B=t(acm)))
}


# SIMPLEX METHOD
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


createTable <- function(indices, nutritionTable, sol){
  len = length(sol)
  ans = sol[(len-length(indices)):len]
  opt = sol[len]
  
  Food = c()
  Servings = c()
  Cost = c()
  for(i in 1:(length(ans)-1)){
    if (ans[i] != 0){
      Food <- append(Food,nutritionTable$Foods[indices[i]])
      Servings <- append(Servings,ans[i])
      Cost <- append(Cost,ans[i]*nutritionTable$Price_Serving[indices[i]])
    }
  }
  df <- data.frame(Food,Servings,Cost)
  return(df)
}

getDietPlan <- function(indices){
  return(createTable(indices, nutritionTable$Foods,Simplex(testRun(indices)$A,F)$basicSolution))
}
