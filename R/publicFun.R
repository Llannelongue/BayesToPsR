#### TRAINING ####

#' Train a BayesToPs model for regression.
#'
#' @param x A DataFrame with n rows and p columns
#' @param y A vector of length n with the labels
#' @param initialPrior A list with an attribute mean (vector of length p)
#' and an attribute variance (square matrix of dimension p).
#' It's the initial prior to be used on the root of the tree.
#' @param normalise a boolean, if TRUE (default) then the data is normalised
#' beforehand.
#' @param features2norm vector containing the columns to normalise.
#' Ignored if normalise=FALSE.
#' @param normaliseY a boolean, if TRUE (default), then the response
#' variable is also normalised. Ignored if normalise=FALSE.
#' @param decay a float between 0 and 1 fixing the decay parameter.
#' @param blrResVariance a float representing the residual variance in the
#' Bayesian Linear Model fitted at each node.
#' @param modelSelection one of "Bayes factors" or "Validation",
#' describe the model selection method used to divide the nodes.
#' @param propagatePosterior a boolean, if TRUE (default),
#' the posterior distribution is propagated to the child nodes,
#' otherwise initialPrior is used as prior.
#' @param weightsMethod one of "lm", "validation", "bayes",
#' "leaf only" or "mean" (default). Decided how to find the final weight of
#' the predictor in each leaf.
#' @param metric_createTree one of "auc", "rmse" (default), "r2" or "mae".
#' The metric used to evaluate the performance of the models
#' when building the tree.
#' @param metric_finalWeights one of "auc", "rmse" (default), "r2" or "mae".
#' The metric used to evaluate the performance of the models
#' when fitting the final weights.
#' @param maxDepthTree int limiting the depth of the tree (default to 10).
#' @param maxSizeSlices int limiting the maximum number of observations to
#' consider when computing the joint marginal likelihood at a node
#' (default to 500).
#' @param maxSplitValueTested int limiting the number of split value
#' to test at each node (default to 50).
#' @param minSizeLeaf int fixing the minimum number of observations
#' from the training set in a leaf (default to 1).
#' @param minNbObsV2 int, only if weightsMethod="validation".
#' @param printProgress boolean, if TRUE (default), the progress of the
#' tree will be displayed.
#'
#' @return A trained BayesToPs model
#' @export
bayesTops <- function(x,y, initialPrior,
                  normalise = TRUE, features2norm = names(x), normaliseY = TRUE,
                  decay = .1, blrResVariance = 1,
                  modelSelection = "Bayes factors", propagatePosterior = TRUE,
                  weightsMethod = "mean",
                  metric_createTree = "rmse", metric_finalWeights = "rmse",
                  maxDepthTree = 10, maxSizeSlices = 500,
                  fracVal1 = .2, fracVal2 = .2,
                  maxSplitValueTested = 50, minSizeLeaf = 2, minNbObsV2 = 1,
                  printProgress = TRUE
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

	if(!(is.numeric(fracVal1) & (fracVal1 > 0) & (fracVal1 <= 1))){
	  stop("Invalid argument fracVal1")
	}

	if(!(is.numeric(fracVal2) & (fracVal2 > 0) & (fracVal2 <= 1))){
	  stop("Invalid argument fracVal2")
	}

	if(!(is.numeric(maxDepthTree) & (maxDepthTree > 0) &
	     (maxDepthTree%%1 == 0) )){
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

	if(!(is.numeric(maxSizeSlices) & (maxSizeSlices > 0) &
	     (maxSizeSlices%%1 == 0) )){
		stop("Invalid argument maxSizeSlices")
	}

	if(!(is.numeric(maxSplitValueTested) & (maxSplitValueTested > 0) &
	     (maxSplitValueTested%%1 == 0) )){
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

	ToP <- btopsTrain(XtrainFull = x, YtrainFull = y,
	                  normalise = normalise,
	                  features2norm = features2norm,
	                  normaliseY = normaliseY,
	                  fracVal1 = fracVal1, fracVal2 = fracVal2,
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


#### PREDICT ####

#' Predict labels using a trained BayesToPs model.
#'
#' @param trained.model Trained model
#' @param mewdata New dataset with the same features
#' @return list with raw and normalised predicted labels
#' @export
bayesTops.predict <- function(trained.model, newdata, confidence.level=.95){
  return(btopsPredict(ToP = trained.model, newdata = newdata,
                      confidence.level=confidence.level))
}

