# Tested and works

btopsNormalise <- function(normalise,
                           XtrainFull, YtrainFull,
                           features2norm,
                           normaliseY){
	if(normalise){
		normalizerMeanX <- base::apply(XtrainFull, 2, base::mean)


		normalizerSdX <- base::apply(XtrainFull, 2, stats::sd)


		normalizerMeanX[!(base::names(normalizerMeanX) %in% features2norm)] <- 0
		normalizerSdX[!(base::names(normalizerSdX) %in% features2norm)] <- 1

		XtrainNorm <- base::as.data.frame(base::scale(XtrainFull,
		                                              center = normalizerMeanX,
		                                              scale = normalizerSdX))

		if(normaliseY){
			normalizerMeanY <- base::mean(YtrainFull)
			normalizerSdY <- stats::sd(YtrainFull)

			YtrainNorm <- base::scale(YtrainFull, center = normalizerMeanY,
			                          scale = normalizerSdY)
		} else{
			normalizerMeanY <- NA
			normalizerSdY <- NA

			YtrainNorm <- YtrainFull
		}

	} else{
		normalizerMeanX <- NA
		normalizerSdX <- NA
		normalizerMeanY <- NA
		normalizerSdY <- NA
		XtrainNorm <- XtrainFull
		YtrainNorm <- YtrainFull
	}

	return(base::list(normalizer = base::list(doNormalise = normalise,
	                                          normalizerMeanX = normalizerMeanX,
	                                          normalizerSdX = normalizerSdX,
	                                          normalizerMeanY = normalizerMeanY,
	                                          normalizerSdY = normalizerSdY),
										XtrainNorm = XtrainNorm,
										YtrainNorm = YtrainNorm
										)
				)
}

btopsNormaliseNewdata <- function(normalizer, newdata, returnDf){

	if(normalizer$doNormalise){
		newdata2 <- scale(newdata,
		                  center = normalizer$normalizerMeanX,
		                  scale = normalizer$normalizerSdX)
	} else{
		newdata2 <- as.matrix(newdata)
	}

	if(returnDf){
		return(as.data.frame(newdata2))
	} else{
		return(newdata2)
	}
}

