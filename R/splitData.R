# Tested and works

splitTrainVal12 <- function(idx, fracVal1, fracVal2) {
    
    n <- base::length(idx)
    
    sizeVal1 <- base::floor(fracVal1 * n)
    sizeVal2 <- base::floor(fracVal2 * n)
    
    sizeTrain <- n - sizeVal1 - sizeVal2
    
    idxShuffled <- base::sample(idx)
    
    # Select training data ----------------------
    
    trainIdx <- idxShuffled[1:sizeTrain]
    
    # Select the validation sets ----------------
    
    if (sizeVal1 > 0) {
        # special case is if the size is equal to 0.
        val1Idx <- idxShuffled[(sizeTrain + 1):(sizeTrain + sizeVal1)]
    } else {
        val1Idx <- c()
    }
    
    if (sizeVal2 > 0) {
        val2Idx <- idxShuffled[(sizeTrain + sizeVal1 + 1):n]
    } else {
        val2Idx <- c()
    }
    
    return(base::list(trainIdx = base::as.numeric(trainIdx), val1Idx = base::as.numeric(val1Idx), val2Idx = base::as.numeric(val2Idx)))
}
