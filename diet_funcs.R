# Requires the sourcing of nutri_table.R
source("nutri_table.R") # Source the nutrition table for constraints and obj func

# Set Up Tableau Function
setUpTableau <- function(indices){
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
  
  return(tableau)
}


# SIMPLEX METHOD
Simplex <- function(tableau){
  r <- nrow(tableau) # Get dimensions
  c <- ncol(tableau)
  
  tab <- list()
  basSol <- list()
  
  while(min(tableau[r,])<0){ # Proceed to another iteration if there's a negative value at A[r,]
    # Get the Basic Solution
    sol <- tableau[r,1:(c-1)]
    sol[c-1] <- tableau[r,c]
    basSol <- append(basSol, sol)
    
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
    
    # Normalize the Pivot Row
    tableau[PR,] <- tableau[PR,]/tableau[PR,PC]
    for(k in 1:r){  # Eliminate A[row,PC] (skip PR)
      if(k!=PR) tableau[k,] <- tableau[k,] - tableau[PR,]*tableau[k,PC]
    }

    tab[[length(tab)+1]] = tableau
  }
  # Get the Basic Solution
  finalSol <- tableau[r,1:(c-1)]
  finalSol[c-1] <- tableau[r,c]
  return(list(finaltableau=tableau, basicSolution=matrix(
  finalSol, nrow=1), Z=tableau[r,c], perIter=list(tab=tab,sol=basSol)))
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

getOptimalMenu <- function(indices){
  tableau <- setUpTableau(indices)
  resultList <- Simplex(tableau)
  table <- createTable(indices, nutritionTable,resultList$basicSolution)
  return(list(menu=table, cost=resultList$Z, perIter=resultList$perIter))
}
