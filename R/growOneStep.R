# Tested and works


applyDecay <- function(prior, decay) {
    if ("variance" %in% base::names(prior)) {
        # ie model with known variance
        newPrior <- base::list(mean = prior$mean, variance = decay * prior$variance)
    } else {
        newPrior <- base::list(m = prior$m, V = decay * prior$V, a = prior$a, b = prior$b)
    }
    
    return(newPrior)
}

growNextGen <- function(modelEntire, XtrainFull, YtrainFull, trainIdx, val1Idx, modelSelection, maxSplitValueTested, minSizeLeaf, blrResVariance, 
    propagatePosterior, initialPrior, bayesian, metric, maxSizeSlices, printProgress, decay) {
    
    # Initialisation
    bestPerf <- modelEntire$perfModel
    isSplitBetter <- 0
    
    bestSplitFeature <- NA
    bestSplitValue <- NA
    
    bestModelLeft <- NA
    bestModelRight <- NA
    
    bestSplitTrainIdxLeft <- NA
    bestSplitTrainIdxRight <- NA
    
    bestSplitVal1IdxLeft <- NA
    bestSplitVal1IdxRight <- NA
    
    ## Loop through the potential split features
    
    splitFeatureList <- base::colnames(XtrainFull)[base::colnames(XtrainFull) != "Intercept"]
    
    for (splitFeature in splitFeatureList) {
        
        potSplitValueList <- base::unique(XtrainFull[trainIdx, splitFeature])
        
        if (base::length(potSplitValueList) == 1) {
            # ie all values are identical We do nothing and move on the next feature
        } else {
            # We remove the maximum value, since picking it as a threshold wouldn't divide the node.
            potSplitValueList <- potSplitValueList[base::which(potSplitValueList < base::max(potSplitValueList))]
            nbPotSplitValue <- base::length(potSplitValueList)
            
            if (nbPotSplitValue <= maxSplitValueTested) {
                # ie if the number of potential thresholds is not too large, we'll test them all
                splitValueList <- potSplitValueList
                nbSplitValue <- nbPotSplitValue
            } else {
                # if the number of values is too large, we randomly sample (maxSplitValueTested) thresholds
                idxSplitValue <- base::sample(base::seq_len(base::length(potSplitValueList)), size = maxSplitValueTested)
                splitValueList <- potSplitValueList[idxSplitValue]
                nbSplitValue <- base::length(splitValueList)
            }
            
            # Loop through the potential thresholds
            for (splitValue in splitValueList) {
                # We split the node
                idxLeftFull <- base::which(XtrainFull[, splitFeature] <= splitValue)
                idxRightFull <- base::which(XtrainFull[, splitFeature] > splitValue)
                
                trainIdxLeft <- base::intersect(idxLeftFull, trainIdx)
                trainIdxRight <- base::intersect(idxRightFull, trainIdx)
                
                val1IdxLeft <- base::intersect(idxLeftFull, val1Idx)
                val1IdxRight <- base::intersect(idxRightFull, val1Idx)
                
                
                if ((modelSelection == "Bayes factors") & (base::min(base::length(trainIdxLeft), base::length(trainIdxRight)) >= minSizeLeaf) | 
                  ((modelSelection == "Validation") & (base::min(base::length(trainIdxLeft), base::length(trainIdxRight), base::length(val1IdxLeft), 
                    base::length(val1IdxRight)) >= minSizeLeaf))) {
                  # If all the subsets have more than (minSizeLeaf) observations We fit the BLR on each child, using the posterior of the parent node as a
                  # prior, or the initial prior
                  if (propagatePosterior) {
                    priorChild <- applyDecay(prior = modelEntire$posterior, decay = decay)
                  } else {
                    priorChild <- initialPrior
                  }
                  
                  # First we look at the splitted models
                  modelSplitLeft <- trainNodeModel(XtrainFull = XtrainFull, YtrainFull = YtrainFull, trainIdx = trainIdxLeft, val1Idx = val1IdxLeft, 
                    bayesian = bayesian, blrResVariance = blrResVariance, metric = metric, prior = priorChild, modelSelection = modelSelection, 
                    maxSizeSlices = maxSizeSlices)
                  
                  modelSplitRight <- trainNodeModel(XtrainFull = XtrainFull, YtrainFull = YtrainFull, trainIdx = trainIdxRight, val1Idx = val1IdxRight, 
                    bayesian = bayesian, blrResVariance = blrResVariance, metric = metric, prior = priorChild, modelSelection = modelSelection, 
                    maxSizeSlices = maxSizeSlices)
                  
                  # Now we look how efficient is the parent's model
                  if (modelSelection == "Bayes factors") {
                    # In this case, it doesn't make sense to consider the parent model (stricly equal to the sum of child models, cf marginal likelihood)
                    modelLeft <- modelSplitLeft
                    isNewBetterLeft <- 1
                    
                    modelRight <- modelSplitRight
                    isNewBetterRight <- 1
                    
                  } else if (modelSelection == "Validation") {
                    
                    # We use the parent model to predict on the child nodes ------------------------
                    
                    predictedEntireLeft <- predictNodeModel(Xnew = XtrainFull[val1IdxLeft, ], posterior = modelEntire$posterior, blrResVariance = blrResVariance, 
                      bayesian = bayesian, computeFullDistribution = FALSE)
                    
                    perfEntireLeft <- perfMetric(trueLabel = YtrainFull[val1IdxLeft], predLabel = predictedEntireLeft$predLabel, metric = metric, 
                      display = FALSE)
                    averagePerfEntireLeft <- NA
                    
                    predictedEntireRight <- predictNodeModel(Xnew = XtrainFull[val1IdxRight, ], posterior = modelEntire$posterior, blrResVariance = blrResVariance, 
                      bayesian = bayesian, computeFullDistribution = FALSE)
                    
                    perfEntireRight <- perfMetric(trueLabel = YtrainFull[val1IdxRight], predLabel = predictedEntireRight$predLabel, metric = metric, 
                      display = FALSE)
                    averagePerfEntireRight <- NA
                    
                    
                    # We compare the inherited model with the new one for both sides --------------
                    
                    if (modelSplitLeft$perf > perfEntireLeft) {
                      # ie a new model works better
                      modelLeft <- modelSplitLeft
                      isNewBetterLeft <- 1
                      
                    } else {
                      # ie the model w/o division works better
                      modelLeft <- base::list(posterior = modelEntire$posterior, perfModel = perfEntireLeft, averagePerf = averagePerfEntireLeft, 
                        predLabelV1 = predictedEntireLeft$predLabel, prior = modelEntire$prior)
                      isNewBetterLeft <- 0
                    }
                    
                    if (modelSplitRight$perf > perfEntireRight) {
                      # ie a new model works better
                      modelRight <- modelSplitRight
                      isNewBetterRight <- 1
                      
                    } else {
                      # ie the model w/o division works better
                      modelRight <- base::list(posterior = modelEntire$posterior, perfModel = perfEntireRight, averagePerf = averagePerfEntireRight, 
                        predLabelV1 = predictedEntireRight$predLabel, prior = modelEntire$prior)
                      isNewBetterRight <- 0
                    }
                  }
                  
                  ## Now we decide if dividing the node improves the model ----------------------
                  
                  if (isNewBetterLeft + isNewBetterRight == 0) {
                    # No need to devide since the inherited model is better in both cases We do nothing
                  } else {
                    # Combined performance of the children --------------------
                    
                    if (modelSelection == "Bayes factors") {
                      perfChildren <- modelLeft$perf + modelRight$perf  # we sum because it's log marginal likelihood
                      
                    } else if (modelSelection == "Validation") {
                      perfChildren <- perfMetric(trueLabel = c(YtrainFull[val1IdxLeft], YtrainFull[val1IdxRight]), predLabel = c(modelLeft$predLabelV1, 
                        modelRight$predLabelV1), metric = metric, display = FALSE)
                    }
                    
                    if ((!base::is.na(perfChildren)) & (perfChildren > bestPerf)) {
                      # ?? / ie is the split better than the previous best (splitFeature, splitValue)
                      
                      # We update the best model ---------------------
                      
                      if (printProgress) {
                        if (isNewBetterRight + isNewBetterLeft == 1) {
                          strParentUsed <- ", modelEntire used"
                        } else {
                          strParentUsed <- ""
                        }
                        
                        base::cat("\tDivided on: ", splitFeature, "/", base::round(splitValue, digits = 4), "(", perfChildren, ", perf improvement = ", 
                          base::round(perfChildren - bestPerf, digits = 4), strParentUsed, ") \n", sep = "")
                      }
                      
                      
                      bestPerf <- perfChildren
                      isSplitBetter <- 1
                      bestSplitFeature <- splitFeature
                      bestSplitValue <- splitValue
                      
                      bestModelLeft <- modelLeft
                      bestModelRight <- modelRight
                      
                      bestSplitTrainIdxLeft <- trainIdxLeft
                      bestSplitTrainIdxRight <- trainIdxRight
                      
                      bestSplitVal1IdxLeft <- val1IdxLeft
                      bestSplitVal1IdxRight <- val1IdxRight
                    } else {
                      # if no improvement, no division we do nothing
                    }
                  }
                  
                } else {
                  # ie not enough observations in a least one subset
                  
                }
            }
        }
    }
    
    return(base::list(isSplitBetter = isSplitBetter, splitFeature = bestSplitFeature, splitValue = bestSplitValue, modelLeft = bestModelLeft, 
        modelRight = bestModelRight, trainIdxLeft = bestSplitTrainIdxLeft, trainIdxRight = bestSplitTrainIdxRight, val1IdxLeft = bestSplitVal1IdxLeft, 
        val1IdxRight = bestSplitVal1IdxRight))
}

