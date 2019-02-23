
#' Train a BayesToPs model for regression.
#' 
#' @param x A DataFrame
#' @param y A vector with the labels
#' @return A trained BayesToPs model
#' @export 
btops <- function(x,y, initialPrior,
                  normalise = TRUE, features2norm = names(x), normaliseY = TRUE,
                  decay = .1, blrResVariance = 1,
                  modelSelection = "Bayes factors", propagatePosterior = TRUE, weightsMethod = "mean",
                  metric_createTree = "rmse", metric_finalWeights = "rmse",
                  maxDepthTree = 10, maxSizeSlices = 500, maxSplitValueTested = 50, minSizeLeaf = 1, minNbObsV2 = 1,
                  printProgress = TRUE,
                  seed = NA
                  ){
	### Test validity of arguments

	n <- base::nrow(x)

	if(n != length(y)){
		stop("X and Y have non-conforming size")
	}

	if(!all(features2norm %in% names(x))){
		print()
		stop("features2norm are not columns of X")
	}

	if(!(is.numeric(fracVal1) & (fracVal1 >= 0) & (fracVal1 <= 1))){
		stop("Invalid argument fracVal1")
	}

	if(!(is.numeric(fracVal2) & (fracVal2 >= 0) & (fracVal2 <= 1))){
		stop("Invalid argument fracVal2")
	}

	if(!(is.numeric(decay) & (decay > 0) & (decay <= 1))){
		stop("Invalid argument decay")
	}

	if(!(is.numeric(maxDepthTree) & (maxDepthTree > 0) & (maxDepthTree%%1 == 0) )){
		stop("Invalid argument maxDepthTree")
	}

	if(!modelSelection %in% c("Bayes factors", "Validation")){
		stop("Invalid argument modelSelection")
	}

	if(!weightsMethod %in% c("lm", "validation", "bayes", "leaf only", "mean")){
		stop("Invalid argument weightsMethod")
	}

	if(!metric_createTree %in% c("auc", "rmse", "r2", "mae")){
		stop("Invalid argument metric_createTree")
	}

	if(!metric_finalWeights %in% c("auc", "rmse", "r2", "mae")){
		stop("Invalid argument metric_finalWeights")
	}

	if(!(is.numeric(maxSizeSlices) & (maxSizeSlices > 0) & (maxSizeSlices%%1 == 0) )){
		stop("Invalid argument maxSizeSlices")
	}

	if(!(is.numeric(maxSplitValueTested) & (maxSplitValueTested > 0) & (maxSplitValueTested%%1 == 0) )){
		stop("Invalid argument maxSplitValueTested")
	}

	if(!(is.numeric(minSizeLeaf) & (minSizeLeaf > 0) & (minSizeLeaf%%1 == 0) )){
		stop("Invalid argument minSizeLeaf")
	}

	if(!(is.numeric(minNbObsV2) & (minNbObsV2 > 0) & (minNbObsV2%%1 == 0) )){
		stop("Invalid argument minNbObsV2")
	}

	if(!(base::isTRUE(normalise)|base::isFALSE(normalise))){
		stop("Invalid argument normalise")
	}

	if(!(base::isTRUE(normaliseY)|base::isFALSE(normaliseY))){
		stop("Invalid argument normaliseY")
	}

	if(!(base::isTRUE(propagatePosterior)|base::isFALSE(propagatePosterior))){
		stop("Invalid argument propagatePosterior")
	}

	if(!(base::isTRUE(printProgress)|base::isFALSE(printProgress))){
		stop("Invalid argument printProgress")
	}

	set.seed()

	ToP <- btopsTrain(XtrainFull = x, YtrainFull = y,
	                  normalise = normalise,
	                  features2norm = features2norm,
	                  normaliseY = normaliseY,
	                  fracVal1 = , fracVal2 = ,
	                  maxDepthTree = maxDepthTree,
	                  blrResVariance = blrResVariance,
	                  bayesian = TRUE,
	                  initialPrior = initialPrior,
	                  modelSelection = modelSelection,
	                  maxSizeSlices = maxSizeSlices,
	                  printProgress = printProgress,
	                  maxSplitValueTested = maxSplitValueTested,
	                  minSizeLeaf = minSizeLeaf,
	                  propagatePosterior = propagatePosterior,
	                  decay = decay,
	                  weightsMethod = weightsMethod,
	                  minNbObsV2 = minNbObsV2,
	                  metric_createTree = metric_createTree,
	                  metric_finalWeights = metric_finalWeights)

	return(ToP)
}




