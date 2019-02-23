# Tested and works

perfMetric <- function(trueLabel, predLabel, metric, display=FALSE){

    if(metric=="auc"){
        perf <- Metrics::auc(trueLabel, predLabel)
        loss <- 1 - perf
    }
    if(metric=="rmse"){
        perf <- -Metrics::rmse(trueLabel, predLabel)
        # We want to maximize (-RMSE)
        loss <- -perf
    }
    if(metric=="r2"){
        loss <- (base::sum((trueLabel-predLabel )^2)/
                   base::sum((trueLabel-base::mean(trueLabel))^2))
        perf <- 1 - loss
    }
    if(metric=="mae"){
        loss <- base::mean(base::abs(trueLabel - predLabel))
        perf <- -loss
    }

    if(display){
        base::cat(metric,":", perf, "\n")
        base::cat("Loss:", loss, "\n")
    }

    return(perf)
}

