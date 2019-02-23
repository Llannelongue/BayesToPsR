# Tested and works

getNextNode <- function(node, obs){
  
  # Depending of the (split value, split Feature) of this node, the next node is it's right or left child

  if(obs[node$splitFeature] <= node$splitValue){
    nextNodeIdx <- node$leftChild

  } else{
    nextNodeIdx <- node$rightChild
  }

  return(nextNodeIdx)
}

getPathSingle <- function(ToP,
                          obs,
                          obsTarget,
                          doPredict){
  
  # Initialisation
  leafNotReached <- TRUE
  
  nodeIdx <- 1 # We start at the root
  
  path <- base::as.list(base::rep(NA, ToP$depthTree)) # list of indexes of nodes on the paths.
  path[[1]] <- nodeIdx # we add the root
  pathIdx <- 2 # we use this index to know where to insert the next node
  
  predictions <- base::as.list(base::rep(NA, ToP$depthTree))
  variancePredictions <- base::as.list(base::rep(NA, ToP$depthTree))
  predIdx <- 1
  
  # print(node_idx)
  
  # We keep going through the tree until a leaf is reached.
  while(leafNotReached){
    # Get the current node from the tree
    node <- ToP$Nodes[[nodeIdx]]
    
    # if doPredict==TRUE, make the prediction on this node 
    if(doPredict){ # ie if we want to get the predictions on the path
      pred <- predictNodeModel(Xnew = t(obs),
                               posterior = node$model$posterior, 
                               blrResVariance = ToP$blrResVariance,
                               bayesian = ToP$bayesian,
                               computeFullDistribution = TRUE)

      predictions[[predIdx]] <- pred$predLabel
      variancePredictions[[predIdx]] <- pred$predDistribution$Sigma_p
      # And update the prediction tracker
      predIdx <- predIdx + 1
    }
  
    # Test if this node is a leaf 
    if(node$isLeaf == 1){ # ie it's a leaf
      leafNotReached <- FALSE

    } else{ # it's not a leaf
      # We get the index of the next node on the path
      nextNodeIdx <- getNextNode(node=node, obs=obs)
      # We add it to the path
      path[pathIdx] <- nextNodeIdx
      # update the pathIdx tracker
      pathIdx <- pathIdx + 1
      # as well as the nodeIdx 
      nodeIdx <- nextNodeIdx
      
    }
  }
  
  
  return(base::list(path = path[!base::is.na(path)],
                    predictions = predictions[!base::is.na(predictions)],
                    variancePredictions = variancePredictions[!base::is.na(variancePredictions)],
                    target = obsTarget))
}

getPathMulti <- function(X, Y, ToP){

  # We apply get_path_ind on each observation
  # we get a list of nrow(df_obs)
  n <- base::nrow(X)
  obsPaths <- base::as.list(base::rep(NA, base::length(n)))
  obsPathsTracker <- 1

  for(idx in 1:n){
    if(base::is.na(Y)[1]){ # if there is no target (prediction on new data)
      obsPaths[[obsPathsTracker]] <- getPathSingle(ToP = ToP,
                                                   obs = X[idx,],
                                                   obsTarget = NA,
                                                   doPredict = TRUE)

    } else{ # ie if there is a target
      obsPaths[[obsPathsTracker]] <- getPathSingle(ToP = ToP,
                                                   obs = X[idx,],
                                                   obsTarget = Y[idx],
                                                   doPredict = TRUE)
    }
    
    obsPathsTracker <- obsPathsTracker + 1
  }
             
  return(obsPaths)
}
