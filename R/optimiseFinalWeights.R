# Tested and works 

addPredVal2 <- function(Xval2, Yval2, ToP){

  obsPaths <- getPathMulti(X = Xval2, 
                           Y = Yval2, 
                           ToP = ToP)
                 
  # We go through the observations stored in obs_paths and add the predictions to ToP 
  for(pathInd in obsPaths){

    # we first get the leaf of this observation 
    leaf <- pathInd$path[[base::length(pathInd$path)]]
    
    # we also get the predictions on the path 
    pred <- pathInd$predictions
    
    # we add the target and unlist everything: Target is the last column 
    predUnlist <- c(base::unlist(pred), pathInd$target)
    
    ## we add the prediction to the corresponding node in ToP
    # we collect the existing prediction tracker
    predVal2Idx <- ToP$Nodes[[leaf]]$nbObsV2 + 1 # Tracket to know where we should insert the data in ToP
    
    # we add the new pred to the node 
    ToP$Nodes[[leaf]]$predLabelVal2[predVal2Idx,] <- base::unlist(predUnlist)

    
    # We update the number of observations in this leaf
    ToP$Nodes[[leaf]]$nbObsV2 <- ToP$Nodes[[leaf]]$nbObsV2 + 1

  }
  
  return(ToP)
}


findFinalWeights <- function(ToP, weightsMethod, metric, minNbObsV2){
  
  for(leaf in ToP$terminalLeaves){
    # We first pull the number of v2-observations available 
    nbObsV2 <- ToP$Nodes[[leaf]]$nbObsV2
    # And the number of predictors associated with this leaf (ie depth of this leaf)
    nbPredictors <- ToP$Nodes[[leaf]]$depth

    leafWeightsMethod <- ""

    if(weightsMethod == "lm"){
      
      if(nbObsV2 <= nbPredictors){ # ie if there is not enough observations to fit a lm (covariance matrix singular)
        # We cannot fit a model for the weights, we leave which_modelWeights = "mean" 
        # For prediction, we'll use a default set of weights 
        leafWeightsMethod <- "mean"

      } else{ # We can fit a lm for weights 
        # We update the leaf with the matrix without NAs
        ToP$Nodes[[leaf]]$predLabelVal2 <- ToP$Nodes[[leaf]]$predLabelVal2[1:nbObsV2,]

        # We pull the  data used to fit the weights convert it to dataframe
        dfLeaf <- base::data.frame(ToP$Nodes[[leaf]]$predLabelVal2)
        # We rename the last column as "Target"
        base::colnames(dfLeaf)[base::ncol(dfLeaf)] <- "Target"
             
        # We fit a new linear model to get the weights 
        modelWeights <- stats::lm(Target~. + 0, data = dfLeaf)
        coefs <- modelWeights$coefficients

        if(base::anyNA(coefs)){ # if there is a singularity and some coef are not defined, we go back to the standard mean 
          leafWeightsMethod <- "mean"
        } else {
          # And add it to the ToP tree 
          ToP$Nodes[[leaf]]$weights <- coefs
          ToP$Nodes[[leaf]]$weightsMethod <- "lm"
        } 
      } 

    } else if (weightsMethod == "validation"){
      if(nbObsV2 <= minNbObsV2){ # ie if there is not enough observations
        leafWeightsMethod <- "mean"
      } else{
        ## We compute the performance on the second validation set 
        # We update the leaf with the matrix without NAs
        ToP$Nodes[[leaf]]$predLabelVal2 <- ToP$Nodes[[leaf]]$predLabelVal2[1:nbObsV2,]

        # We pull the  data used to fit the weights convert it to dataframe
        dfLeaf <- base::data.frame(ToP$Nodes[[leaf]]$predLabelVal2)
        # We rename the last column as "Target"
        base::colnames(dfLeaf)[base::ncol(dfLeaf)] <- "Target"

        # We compute the performance on the dataset V2
        perfVal2 <- base::as.list(base::rep(NA, base::ncol(dfLeaf) -1))

        for(i in 1:(ncol(dfLeaf)-1)){
          perfVal2[[i]] <- perfMetric(trueLabel = dfLeaf$Target, 
                                      predLabel = dfLeaf[,i], 
                                      metric = metric, 
                                      display=FALSE)
        }

        ToP$Nodes[[leaf]]$weights <- base::unlist(perfVal2)/base::sum(base::unlist(perfVal2))
        ToP$Nodes[[leaf]]$weightsMethod <- "validation"  
      }
      
    } else if (weightsMethod == "bayes"){
      path <- ToP$Nodes[[leaf]]$path2root

      rawWeights <- base::rep(NA, base::length(path))
      tracker <- 1

      for(node in path){
        rawWeights[tracker] <- ToP$Nodes[[node]]$model$averagePerf
        tracker <- tracker + 1
      }

      ## We normalise the weights
      # First we compute the log of the sum of the margina likelihoods
      normConstant <- matrixStats::logSumExp(rawWeights)
      # We normalise the log weights
      logWeights <- rawWeights - normConstant
      # We find the weights
      weights <- base::exp(logWeights)

      ToP$Nodes[[leaf]]$weights <- weights
      ToP$Nodes[[leaf]]$weightsMethod <- "bayes"

    } else if(weightsMethod == "leaf only"){
      weights <- base::rep(0, nbPredictors)
      weights[nbPredictors] <- 1

      ToP$Nodes[[leaf]]$weights <- weights
      ToP$Nodes[[leaf]]$weightsMethod <- "leaf only"
    }

    if((leafWeightsMethod == "mean") || (weightsMethod == "mean")){
      ToP$Nodes[[leaf]]$weights <- base::rep(1/nbPredictors, nbPredictors)
      ToP$Nodes[[leaf]]$weightsMethod <- "mean"
    }

  }
  
  return(ToP)
}
