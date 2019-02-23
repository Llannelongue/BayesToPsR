# Tested and works

initialiseNode <- function(trainIdx, val1Idx, model, depthParent,
                           sizeVal2, path2root){

    return(base::list("leftChild" = NA,
                      "rightChild" = NA,
                      "splitFeature" = NA,
                      "splitValue" = NA,
                      "trainIdx" = trainIdx,
                      "val1Idx" = val1Idx,
                      "model" = model,
                      "isLeaf" = 1,
                      "path2root" = path2root,
                      "depth" = depthParent + 1,
                      "predLabelVal2" = matrix(NA, nrow = sizeVal2,
                                               ncol = depthParent + 2),
                      "nbObsV2" = 0,
                      "weights" = NA,
                      "weightsMethod" = NA
                      ))
}


addChildren <- function(ToP, nextGen, parentIdx, sizeVal2, temporaryLeavesIdx){

  leftChildIdx <- ToP$nbNodes + 1
  rightChildIdx <- ToP$nbNodes + 2

  depthParent <- ToP$Nodes[[parentIdx]]$depth

  path2rootParent <- ToP$Nodes[[parentIdx]]$path2root

  # Add the child nodes to the tree -------------------------------------------

  ToP$Nodes[[leftChildIdx]] <- initialiseNode(trainIdx = nextGen$trainIdxLeft,
                                              val1Idx = nextGen$val1IdxLeft,
                                              model = nextGen$modelLeft,
                                              depthParent = depthParent,
                                              sizeVal2 = sizeVal2,
                                              path2root = c(path2rootParent,
                                                            leftChildIdx))

  ToP$Nodes[[rightChildIdx]] <- initialiseNode(trainIdx = nextGen$trainIdxRight,
                                               val1Idx = nextGen$val1IdxRight,
                                               model = nextGen$modelRight,
                                               depthParent = depthParent,
                                               sizeVal2 = sizeVal2,
                                               path2root = c(path2rootParent,
                                                             rightChildIdx))

  # Add the split/children information to the parent node  ---------------------

  ToP$Nodes[[parentIdx]]$leftChild <- leftChildIdx
  ToP$Nodes[[parentIdx]]$rightChild <- rightChildIdx
  ToP$Nodes[[parentIdx]]$splitFeature <- nextGen$splitFeature
  ToP$Nodes[[parentIdx]]$splitValue <- nextGen$splitValue
  ToP$Nodes[[parentIdx]]$isLeaf <- 0
  ToP$Nodes[[parentIdx]]$predLabelVal2 <- NA
    # Since it's not a leaf, there is no use for v2-predictions

  # Update the list of leaves to be tested -------------------------------------

  # First remove the parent's index
  temporaryLeavesIdx <- base::setdiff(temporaryLeavesIdx, parentIdx)
  # Then add the children's indexes
  temporaryLeavesIdx <- c(temporaryLeavesIdx, leftChildIdx, rightChildIdx)

  # We update the number of nodes:
  ToP$nbNodes <- ToP$nbNodes + 2

  return(base::list(ToP = ToP,
                    temporaryLeavesIdx = temporaryLeavesIdx))
}

createTree <- function(XtrainFull, YtrainFull,
                       trainIdx, val1Idx, val2Idx,
                       normalizer,
                       blrResVariance,
                       bayesian,
                       metric,
                       initialPrior,
                       modelSelection,
                       maxSizeSlices,
                       maxDepthTree,
                       printProgress,
                       maxSplitValueTested,
                       minSizeLeaf,
                       propagatePosterior,
                       decay){


  # Creates the tree structure ---------------------

  ToP <- base::list(Nodes = base::as.list(base::rep(NA,
                                                    2 ^ (maxDepthTree +
                                                           1) - 1)),
                    blrResVariance = blrResVariance,
                    terminalLeaves = c(), # terminal leaves' indexes
                    nbNodes = 0, # Number of nodes
                    depthTree = 0, # depth of the tree (= number of "layers")
                    bayesian = bayesian
                      # whether or not it's the bayesian version of ToP
                    )

  ToP$normalizer <- normalizer

  # Create the root and add it to the tree ---------

  modelRoot <- trainNodeModel(XtrainFull = XtrainFull, YtrainFull = YtrainFull,
                              trainIdx = trainIdx,
                              val1Idx = val1Idx,
                              bayesian = bayesian,
                              blrResVariance = blrResVariance,
                              metric = metric,
                              prior = initialPrior,
                              modelSelection = modelSelection,
                              maxSizeSlices = maxSizeSlices)

  ToP$Nodes[[1]] <- initialiseNode(trainIdx = trainIdx,
                                   val1Idx = val1Idx,
                                   model = modelRoot,
                                   depthParent = 0,
                                   sizeVal2 = base::length(val2Idx),
                                   path2root = c(1))
  ToP$nbNodes <- ToP$nbNodes + 1
  ToP$depthTree <- ToP$depthTree + 1

  temporaryLeavesIdx <- c(1) #Initialise the vector storing the leaves to visit

  # Build the tree further until there is no leaf in temporaryLeavesIdx --------

  while (base::length(temporaryLeavesIdx) > 0){

    # We take as a parent node the first element of temporaryLeavesIdx
    parentIdx <- temporaryLeavesIdx[1]
    parentDepth <- ToP$Nodes[[parentIdx]]$depth

    # We check if this node is already at the maximum depth allowed ------------
    if (parentDepth >= maxDepthTree){
      # We remove the node from the temporaryLeavesIdx, and keep Nodes as it is.
      temporaryLeavesIdx <- base::setdiff(temporaryLeavesIdx, parentIdx)

      # And add this node to the list of terminal leaves
      ToP$terminalLeaves <- c(ToP$terminalLeaves, parentIdx)

      if (printProgress){
        base::cat("\t---> node",parentIdx,": maxDepth reached \n\n")
      }


    } else{ # we can potentially add children

      nextGen <- growNextGen(modelEntire = ToP$Nodes[[parentIdx]]$model,
                             XtrainFull = XtrainFull, YtrainFull = YtrainFull,
                             trainIdx = ToP$Nodes[[parentIdx]]$trainIdx,
                             val1Idx =  ToP$Nodes[[parentIdx]]$val1Idx,
                             modelSelection = modelSelection,
                             maxSplitValueTested = maxSplitValueTested,
                             minSizeLeaf = minSizeLeaf,
                             blrResVariance = blrResVariance,
                             propagatePosterior = propagatePosterior,
                             initialPrior = initialPrior,
                             bayesian = bayesian,
                             metric = metric,
                             maxSizeSlices = maxSizeSlices,
                             printProgress = printProgress,
                             decay = decay)

      # We decide to split or not this node
      if (nextGen$isSplitBetter == 0){ # ie if the root is NOT divided
        # We remove the node from temporaryLeavesIdx, and keep Nodes as it is.
        temporaryLeavesIdx <- base::setdiff(temporaryLeavesIdx, parentIdx)

        # And add this node to the list of terminal leaves
        ToP$terminalLeaves <- c(ToP$terminalLeaves, parentIdx)

        if(printProgress){
          base::cat("\t---> node",parentIdx,
                    ": not divided (performance or minSizeLeaf) \n\n")
        }

      } else { # if the node is divided
        # We update Nodes and temporaryLeavesIdx with the children.
        ToPwithKids <- addChildren(ToP = ToP,
                                    nextGen = nextGen,
                                    parentIdx = parentIdx,
                                    sizeVal2 = base::length(val2Idx),
                                    temporaryLeavesIdx = temporaryLeavesIdx)

        ToP <- ToPwithKids$ToP
        temporaryLeavesIdx <- ToPwithKids$temporaryLeavesIdx

        # We also update the depth of the tree if necessary
        if(parentDepth + 1 > ToP$depthTree){
          ToP$depthTree <- parentDepth + 1
        }

        if(printProgress){
          if(ToP$normalizer$doNormalise){
            newSplitValue <- nextGen$splitValue *
              ToP$normalizer$normalizerSdX[nextGen$splitFeature] +
              ToP$normalizer$normalizerMeanX[nextGen$splitFeature]
          } else{
            newSplitValue <- nextGen$splitValue
          }
          base::cat("\t---> node",parentIdx,": divided on",
                    nextGen$splitFeature, "/", newSplitValue, "\n\n")
        }

      }
    }
  }

  # Remove the NAs from the list of nodes
  ToP$Nodes <- ToP$Nodes[!base::is.na(ToP$Nodes)]

  base::cat("\n\t# Depth of the tree:", ToP$depthTree)
  base::cat("\n\t# Number of nodes:", ToP$nbNodes, "\n")

  return(ToP)
}
