# Tested and works

predictNodeModel <- function(Xnew,
                             posterior,
                             blrResVariance,
                             bayesian,
                             computeFullDistribution){

    # BToPs -------------------

    if(bayesian){

        # Model with known variance ----------------

        if(!base::is.na(blrResVariance)){
            mu_p <- Xnew %*% posterior$mean

            if(computeFullDistribution){
                Sigma_p <- 1/blrResVariance +
                  base::diag(Xnew %*% posterior$variance %*% t(Xnew))
            } else{
                Sigma_p <- NA
            }

            predDistribution <- base::list(mu_p = mu_p,
                                           Sigma_p = Sigma_p)

        # Model with unknown variance --------------

        } else{
            n <- base::nrow(Xnew) # always equal to 1?

            mu_p <- Xnew %*% posterior$m

            if(computeFullDistribution){
                a_p <- 2 * posterior$a
                Sigma_p <- posterior$b / posterior$a *
                  (base::diag(n) + Xnew %*% posterior$V %*% t(Xnew))
            } else {
                a_p <- NA
                Sigma_p <- NA
            }

            predDistribution <- base::list(mu_p = mu_p,
                                           Sigma_p = Sigma_p,
                                           a_p = a_p)
        }

    # ToPs/LR -------------------

    } else{

        mu_p <- Xnew %*% posterior$mean

        if(computeFullDistribution){
            Sigma_p <- 1/blrResVariance +
              base::diag(Xnew %*% posterior$variance %*% t(Xnew))
        } else{
            Sigma_p <- NA
        }

        predDistribution <- base::list(mu_p = mu_p,
                                       Sigma_p = Sigma_p,
                                       df = posterior$df)
    }

    return(base::list(predLabel = mu_p,
                      predDistribution = predDistribution))
}


computeMarginalLikelihood <- function(X, Y,
                                      prior,
                                      maxSizeSlices,
                                      blrResVariance){
    n <- base::nrow(X)

    # Model with known variance -------------------
    if(!base::is.na(blrResVariance)){

        m0 <- prior$mean
        S0 <- prior$variance

        if(n > maxSizeSlices){
          # data too large to be evaluated in one time, we assume independance
            nbSlices <- base::ceiling(n / maxSizeSlices)

            startSlice <- 1
            perfModel <- 0

            for(i in 1:nbSlices){

                if(startSlice + maxSizeSlices - 1 <= n){
                    endSlice <-  startSlice+maxSizeSlices-1
                } else{
                    endSlice <- n
                }

                if(startSlice == endSlice){

                    X_i <- base::matrix(X[startSlice,], nrow = 1)

                } else{

                    X_i <- X[startSlice:endSlice,]

                }
                Y_i <- Y[startSlice:endSlice]

                perf_i <- mvtnorm::dmvnorm(Y_i,
                                           mean = as.vector(X_i %*% m0),
                                           sigma = 1/blrResVariance *
                                             base::diag(endSlice -
                                                          startSlice + 1) +
                                             X_i %*% S0 %*% base::t(X_i),
                                           log=TRUE)

                startSlice <- startSlice+maxSizeSlices
                perfModel <- perfModel + perf_i
            }

        } else{ # ie data is small enough
            perfModel <- mvtnorm::dmvnorm(Y,
                                          mean = as.vector(X %*% m0),
                                          sigma = 1/blrResVariance *
                                            base::diag(n) +
                                            X %*% S0 %*% base::t(X),
                                          log=TRUE)
        }

        averagePerf <- perfModel / n # Average log marginal likelihood per obs

    } else { # ie model with unknown variance
        m0 <- prior$m
        V0 <- prior$V
        a0 <- prior$a
        b0 <- prior$b

        if(n>maxSizeSlices){
          # data too large to be evaluated at one, we assume independance here
            nbSlices <- base::ceiling(n / maxSizeSlices)

            startSlice <- 1
            perfModel <- 0

            for(i in 1:nbSlices){

                if(startSlice + maxSizeSlices-1 <= n){
                    endSlice <-  startSlice + maxSizeSlices-1
                } else{
                    endSlice <- n
                }

                if(startSlice == endSlice){
                    X_i <- base::matrix(X[startSlice,], nrow=1)
                } else{
                    X_i <- X[startSlice:endSlice,]
                }
                Y_i <- Y[startSlice:endSlice]

                sig0_i <- X_i %*% V0 %*% t(X_i)
                sig_i <- b0 / a0 * (base::diag(endSlice - startSlice + 1) +
                                      sig0_i)
                perf_i <- mvtnorm::dmvt(Y_i,
                                        delta = X_i %*% m0,
                                        sigma = sig_i,
                                        df = 2*a0,
                                        log=TRUE)

                startSlice <- startSlice + maxSizeSlices
                perfModel <- perfModel + perf_i
            }

        } else{ # ie data is small enough
            sig0 <- X %*% V0 %*% base::t(X)
            sig <- b0 / a0 * (base::diag(n) + sig0)
            perfModel <- mvtnorm::dmvt(Y,
                                        delta = X %*% m0,
                                        sigma = sig,
                                        df = 2*a0,
                                        log=TRUE)
        }

        averagePerf <- perfModel / n
          # Average log marginal likelihood per observation
    }

    return(base::list(perfModel = perfModel,
                      averagePerf = averagePerf))
}



trainNodeModel <- function(XtrainFull, YtrainFull,
                           trainIdx,
                           val1Idx,
                           bayesian,
                           blrResVariance,
                           metric,
                           prior,
                           modelSelection,
                           maxSizeSlices){

    Xtrain <- XtrainFull[trainIdx,]
    Ytrain <- YtrainFull[trainIdx]

    Xv1 <- XtrainFull[val1Idx,]
    Yv1 <- YtrainFull[val1Idx]

    p <- base::ncol(Xtrain)
    n <- base::nrow(Xtrain)

    # Compute the ToPs/LR model ------------------

    if(!bayesian){

        lm <- stats::lm.fit(x = Xtrain, y=Ytrain)

        m1 <- lm$coefficients
        m1[base::is.na(m1)] <- 0
          # We set the undefined coefficients (usually rank issue) to 0

        tXX <- base::t(Xtrain) %*% Xtrain

        S1 <- base::tryCatch(
          # For the cases where the variance matrix is not invertible
                {
                    base::solve(blrResVariance * tXX)
                },
                error = function(error_condition){
                  return(MASS::ginv(blrResVariance * tXX))
                  }
                )

        posterior <- base::list(mean = m1,
                                variance = S1,
                                df = n-p)

        predictedModel <- predictNodeModel(Xnew = Xv1,
                                           posterior = posterior,
                                           blrResVariance = blrResVariance,
                                           bayesian = bayesian,
                                           computeFullDistribution = FALSE)

        perfModel <- perfMetric(trueLabel = Yv1,
                                predLabel = predictedModel$predLabel,
                                metric=metric,
                                display=FALSE)
        averagePerf <- NA

        predLabelV1 <- predictedModel$predLabel

    # Compute the BToPs model ------------------------

    } else{

        # Model with known variance -------------------

        if(!base::is.na(blrResVariance)){
            m0 <- prior$mean
            S0 <- prior$variance

            # Compute Posterior distribution ----------
            invS0 <- base::solve(S0)
            S1 <- base::solve(invS0 + blrResVariance *
                                base::t(Xtrain) %*% Xtrain)
            m1 <- S1 %*% (blrResVariance * base::t(Xtrain)
                          %*% Ytrain + invS0 %*% m0)

            posterior <- base::list(mean = m1,
                                    variance = S1)

        # Model with unknown variance -------------------

        } else {

            m0 <- prior$m
            V0 <- prior$V
            a0 <- prior$a
            b0 <- prior$b

            # Compute Posterior distribution ----------

            invV0 <- base::solve(V0) # We do that to speed up computation
            invV1 <- invV0 + base::t(Xtrain) %*% Xtrain

            V1 <- base::solve(invV1)
            m1 <- V1 %*% (invV0 %*% m0 + base::t(Xtrain) %*% Ytrain)

            a1 <- a0 + n/2
            b1 <- base::as.numeric(b0 + 1/2 * (base::t(m0)
                                               %*% invV0 %*% m0 +
                                                 base::t(Ytrain) %*% Ytrain -
                                                 base::t(m1) %*% invV1 %*% m1))

            posterior <- base::list(m = m1,
                                    V = V1,
                                    a = a1,
                                    b = b1)
        }

        if(modelSelection=="Bayes factors"){

            margLklh <- computeMarginalLikelihood(X = Xtrain, Y = Ytrain,
                                                  prior=prior,
                                                  maxSizeSlices = maxSizeSlices,
                                                  blrResVariance=blrResVariance)

            perfModel <- margLklh$perfModel
            predictedModel <- base::list(predLabel = NA,
                                         predDistribution = NA)
            averagePerf <- margLklh$averagePerf
            predLabelV1 <- NA
        }

        if(modelSelection=="Validation"){
            predictedModel <- predictNodeModel(Xnew = Xv1,
                                               posterior = posterior,
                                               blrResVariance = blrResVariance,
                                               bayesian = bayesian,
                                               computeFullDistribution = FALSE)

            predLabelV1 <- predictedModel$predLabel

            perfModel <- perfMetric(trueLabel = Yv1,
                                    predLabel = predLabelV1,
                                    metric=metric,
                                    display=FALSE)
            averagePerf <- NA

        }
    }


    return(base::list(posterior = posterior,
                      perfModel = perfModel,
                      averagePerf = averagePerf,
                      predLabelV1 = predLabelV1,
                      prior = prior))
}
