
# Tested and works

btopsTrain <- function(XtrainFull, YtrainFull,
                       normalise,
                       features2norm,
                       normaliseY,
                       fracVal1, fracVal2,
                       maxDepthTree,
                       blrResVariance,
                       bayesian,
                       initialPrior,
                       modelSelection,
                       maxSizeSlices,
                       printProgress,
                       maxSplitValueTested,
                       minSizeLeaf,
                       propagatePosterior,
                       decay,
                       weightsMethod,
                       minNbObsV2,
                       metric_createTree,
                       metric_finalWeights){


  n <- base::nrow(XtrainFull)

	base::rownames(XtrainFull) <- 1:n

  # Set aside the validation sets ---------------------

	newFracVal <- findV1V2(modelSelection = modelSelection,
	                       weightsMethod = weightsMethod,
	                       fracVal1=fracVal1, fracVal2=fracVal2)

  trainValIdx <- splitTrainVal12(idx = 1:n, fracVal1 = newFracVal$fracVal1bis,
                                 fracVal2 = newFracVal$fracVal2bis)
  trainIdx <- trainValIdx$trainIdx
  val1Idx <- trainValIdx$val1Idx
  val2Idx <- trainValIdx$val2Idx
	# Normalise data --------

	normalisedData <- btopsNormalise(normalise = normalise,
	                                 XtrainFull = XtrainFull,
	                                 YtrainFull = YtrainFull,
	                                 features2norm = features2norm,
	                                 normaliseY = normaliseY)

	X2trainFull <- base::cbind("Intercept" = 1,
	                           base::as.matrix(normalisedData$XtrainNorm))
	Y2trainFull <- base::as.matrix(normalisedData$YtrainNorm)

	base::cat("\n1. Create the tree\n")

	ToP <- createTree(XtrainFull = X2trainFull, YtrainFull = Y2trainFull,
	                  trainIdx = trainIdx, val1Idx = val1Idx, val2Idx = val2Idx,
	                  normalizer = normalisedData$normalizer,
	                  blrResVariance = blrResVariance,
	                  bayesian = bayesian,
	                  metric = metric_createTree,
	                  initialPrior = initialPrior,
	                  modelSelection = modelSelection,
	                  maxSizeSlices = maxSizeSlices,
                    maxDepthTree = maxDepthTree,
                    printProgress = printProgress,
                    maxSplitValueTested = maxSplitValueTested,
                    minSizeLeaf = minSizeLeaf,
                    propagatePosterior = propagatePosterior,
                    decay = decay)

	if(weightsMethod %in% c("lm", "validation")){

		tictoc::tic("Add val2 predictions")
		base::cat("\n1bis. Add predictions on Val2 to the tree \n")

		ToP <- addPredVal2(Xval2 = X2trainFull[val2Idx,],
		                   Yval2 = Y2trainFull[val2Idx],
		                   ToP = ToP)

		tictoc::toc()
	}

	tictoc::tic("Find weights")
	base::cat("\n2. Find the optimal weights\n")
	ToP <- findFinalWeights(ToP = ToP,
	                        weightsMethod = weightsMethod,
	                        metric = metric_finalWeights,
	                        minNbObsV2 = minNbObsV2)
	tictoc::toc()

	tictoc::tic("Cleaning")
	base::cat("\n3. Remove unnecessary data from the tree \n")
	for (i in 1:base::length(ToP$Nodes)){
	  # (?) should I replace the for loop by an apply
		ToP$Nodes[[i]]$trainIdx <- -1
		ToP$Nodes[[i]]$val1Idx <- -1
		ToP$Nodes[[i]]$model$predLabelV1 <- -1
		ToP$Nodes[[i]]$predLabelVal2 <- -1
	}
	tictoc::toc()
	base::cat("\n")
	# toc()

	return(ToP)
}
