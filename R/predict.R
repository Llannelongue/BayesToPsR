
# Tested and works

computeOverallPredictor <- function(nodePreds, nodeVars, weights, bayesian){

	predictedValue <- weights %*% nodePreds

	predictedVariance <- 0

	for (i in 1:length(nodeVars)){
		predictedVariance <- predictedVariance + weights[i]^2 * nodeVars[[i]]
	}

	return(list(predictedValue = predictedValue,
	            predictedVariance = unname(predictedVariance)))
}

btopsPredict <- function(ToP, newdata, confidence.level){

	# tic("Prediction")
	newdata2 <- base::cbind("Intercept" = 1,
	                        btopsNormaliseNewdata(normalizer=ToP$normalizer,
	                                              newdata=newdata,
	                                              returnDf=FALSE))

	n <- nrow(newdata2)

	predictedValues <- as.list(rep(NA, size = n))
	predictedVariances <- as.list(rep(NA, size = n))
	predIdx <- 1 # tracker

	# if there is a single node in the tree
	if(ToP$nbNodes == 1){

		for(idx in 1:n){
			pred <- predictNodeModel(Xnew = matrix(newdata2[idx,], nrow=1),
			                         posterior = ToP$Nodes[[1]]$model$posterior,
			                         blrResVariance = ToP$blrResVariance,
			                         bayesian = ToP$bayesian,
			                         computeFullDistribution = TRUE)

			predictedValues[[idx]] <- pred$predLabel
			predictedVariances[[idx]] <- pred$predDistribution$Sigma_p
		}

	} else{ # ie if there is more than one node in the tree
		obsPaths <- getPathMulti(X = newdata2, Y=NA, ToP = ToP)

		# We go through the observations stored in obs_paths
		for(pathInd in obsPaths){
			# we first get the leaf of this observation
			leaf <- pathInd$path[[length(pathInd$path)]]

			overallPred <- computeOverallPredictor(nodePreds =
			                                         unlist(pathInd$predictions),
			                                       nodeVars =
			                                         pathInd$variancePredictions,
			                                       weights =
			                                         ToP$Nodes[[leaf]]$weights,
			                                       bayesian =
			                                         oP$bayesian)

			predictedValues[[predIdx]] <- overallPred$predictedValue
			predictedVariances[[predIdx]] <- overallPred$predictedVariance
			predIdx <- predIdx + 1
		}
	}

	# toc()

	if(ToP$normalizer$doNormalise){
		rawPredictedValues <- unlist(predictedValues) *
		  ToP$normalizer$normalizerSdY +
		  ToP$normalizer$normalizerMeanY
		rawPredictedVariances <- unlist(predictedVariances) *
		  ToP$normalizer$normalizerSdY^2
	} else{
		rawPredictedValues <- unlist(predictedValues)
		rawPredictedVariances <- unlist(predictedVariances)
	}

	q <- stats::qnorm(confidence.level)

	PI <- data.frame("lwr" = rawPredictedValues - q * base::sqrt(rawPredictedVariances),
	                 "upr" = rawPredictedValues + q * base::sqrt(rawPredictedVariances))

	return(list(predictedValues = unlist(predictedValues),
	            rawPredictedValues = rawPredictedValues,
	            predictedVariances = unlist(predictedVariances),
	            rawPredictedVariances = rawPredictedVariances,
	            predictiveInterval = PI))
}
