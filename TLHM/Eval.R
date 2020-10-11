

eval <- function(trueValue, prdtValue){
  
  rslt.err <- trueValue - prdtValue
  ## Mean prediction error (me)
  rslt.me <- mean(rslt.err)
  
  ## Median prediction error (mde)
  rslt.mde <- median(rslt.err)
  
  ## Standard deviation of prediction error (sde)
  rslt.sde <- sd(rslt.err)
  
  ## Root mean squared prediction error (rmse)
  rslt.rmse <- rmse(actual = trueValue, predicted = prdtValue)
  
  ## Root median squared prediction error (rdmse)
  rslt.rdmse <- sqrt(median(rslt.err^2))
  
  ## Mean absolute value prediction error (mave)
  rslt.mave <- mean(abs(rslt.err))
  
  ## Median absolute value prediction error (mdave)
  rslt.mdave <- median(abs(rslt.err))
  
  ## Mean absolute value percent error (mavpe)
  rslt.mavpe <- mean(abs(rslt.err/trueValue))
  
  ## Median absolute value percent error (mdavpe)
  rslt.mdavpe <- median(abs(rslt.err/trueValue))
  
  ##prediction accuracy within 5%
  rslt_pr_5 = abs((prdtValue - trueValue)/trueValue)
  rslt_pr_5 = sum(rslt_pr_5 < 0.05)/length(rslt_pr_5)
  
  ##prediction accuracy within 10%
  rslt_pr_10 = abs((prdtValue - trueValue)/trueValue)
  rslt_pr_10 = sum(rslt_pr_10 < 0.1)/length(rslt_pr_10)
  
  ##prediction accuracy within 15%
  rslt_pr_15 = abs((prdtValue - trueValue)/trueValue)
  rslt_pr_15 = sum(rslt_pr_15 < 0.15)/length(rslt_pr_15)
  
  ##prediction accuracy within 20%
  rslt_pr_20 = abs((prdtValue - trueValue)/trueValue)
  rslt_pr_20 = sum(rslt_pr_20 < 0.2)/length(rslt_pr_20)
  
  
  rslt <- c(rslt.me, rslt.mde, rslt.sde, rslt.rmse, rslt.rdmse, rslt.mave, 
            rslt.mdave, rslt.mavpe, rslt.mdavpe, rslt_pr_5, rslt_pr_10, rslt_pr_15, rslt_pr_20)
  
  names(rslt) <- c("me", "mde", "sde", "rmse", "rdmse", "mave", "mdave", 
                   "mavpe", "mdavpe", "pr5", "pr10", "pr15", "pr20")
  
  return(rslt)
}






















