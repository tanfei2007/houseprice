
geneS <- function(data, group, period){
	#input: data/dataframe
	#input: group/vector
	#input: period/vector
	#ouptut: indicator matrix
	
	M = length(group)
	TT = length(period)
	
	#browser()
	
	mat = matrix(0, M, TT)
	
	rownames(mat) = group
	colnames(mat) = period
	
	idx_groups <- match(data$CommunityID, group)
	idx_periods <- match(data$TradingYear, period)
	idx <- (idx_periods - 1)*M + idx_groups
	mat[idx] <- 1
	
	return(mat)
}


geneS_tilde <- function(S){
	#input: indMatrix/matrix
	#output: mat/matrix
	#goal: record cumulative transaction counts with missing fields after transaction supported
		
	S_tilde <- S
	
	for (j in 2:ncol(S_tilde)){
	  S_tilde[,j] <- rowSums(S[,1:j])
	}
	
	S_tilde <- (1 - S) * S_tilde
	
	S_tilde[S_tilde > 0] <- 1
	
	return(S_tilde)
}

#*********************************************
#generate matrix D
#*********************************************
generate_D <- function(nb, S){
  #make sure rownames of D and names of nb are same
  idx <- match(rownames(S), names(nb))
  nb <- nb[idx]
  
  D <- sapply(nb, function(x){
    rslt <- rep(200, length(nb))
    idx <- match(names(x), rownames(S))
    rslt[idx] <- x
    return(rslt)
  })
  D <- t(D)
  diag(D) <- 0 # set diagonal elements to be zero
  rownames(D) <- rownames(S)
  colnames(D) <- rownames(S)
  return(D)
}

#*********************************************
#generate matrix A
#*********************************************
generate_A <- function(D, S, K, p, q){
  
  #initialize A (list), each element is a MATRIX
  group <- rownames(S)
  period <- colnames(S)
  M <- length(group)
  TT <- length(period)
  A <- vector('list', TT)
  names(A) = period
  
  for(ts in 1:TT){
    print(ts)
    Ats <- matrix(0, M, M)
    rownames(Ats) <- group
    colnames(Ats) <- group
    
    numer <- t(S[,ts] * t(exp(-q*(D^p))))
    numer_topK <- apply(numer, 1, function(x){
      idx <- order(x, decreasing = T)
      idx_topK <- idx[1:K]
      x[-idx_topK] <- 0 # set all non-topK elemnts to be zero
      return(x)
    })
    numer_topK <- t(numer_topK)
    diag(numer_topK) <- 0
    Ats <- numer_topK/rowSums(numer_topK) # normalized
    Ats[is.na(Ats)] <- 0
    A[[ts]] <- Ats
  }
  return(A)
}


#**********************
# initialize matrix B
#*********************
initB <- function(A, U){
  group <- rownames(U)
  period <- colnames(U)
  TT <- length(period)
  
  B <- matrix(0, length(group), TT)
  
  for(ts in 1:TT){
    B[,ts] <- A[[ts]] %*% U[,ts]
  }
  return(B)
}



#****************
# initialize U
#****************
initUFilled <- function(data, S){
	#input: data/dataframe
	#input: indMatrix/indicator matrix
	#output: initilization of the neighborhoods value
	
	group = rownames(S)
	period = colnames(S)
	
	U = matrix(0, length(group), length(period))
	
	U_group_period <- data %>%
	  dplyr::select(resp, CommunityID, TradingYear) %>%
	  group_by(CommunityID, TradingYear) %>%
	  summarise(avg_resp = mean(resp))
	
	U_group_period = data.frame(U_group_period)
	
	for(k in 1:nrow(U_group_period)){
		
		i = which(group %in% U_group_period$CommunityID[k])
		t = which(period %in% U_group_period$TradingYear[k])
		u = U_group_period$avg_resp[k]
		
		U[i,t] = u
	}
	
	rownames(U) = group
	colnames(U) = period
	
	return(U)  
}


#**********************************
# initalize missing values
#**********************************
initUMissing <- function(UFilled, S_tilde, B, Phi){
  #input: UFilled/matrix 
  #input: Phi/initial parameters
  #output: initilization and prediction of the neighborhoods value with the support of filled value
  gamma0 <- Phi[1]
  gamma1 <- Phi[2]
  
  M <- nrow(UFilled)
  TT <- ncol(UFilled)
  
  UMissing <- matrix(0, M, TT) #Missing value will be imputed by prediction

  for(ts in 1:TT){
    idx_Missing <- which(S_tilde[,ts] == 1)
    if(length(idx_Missing) > 0 ){
      UMissing[idx_Missing, ts] <- gamma0*B[idx_Missing, ts] + gamma1*UFilled[idx_Missing,ts-1]
    }
  }
  
  return(UMissing)
}


initPhi <- function(){
	#input: UFilled/neighborhoods value with transaction
	#output: Phi/parameters 
	
	gamma0 = 0.5 #the weight for historical values in neighboring neighbors
	gamma1 = 0.5 #the weight for its historical values
	
	Phi = c(gamma0, gamma1)
	names(Phi) = c("gamma0", "gamma1")
	return(Phi)
}


#**************************************
# estimate parameters gamma0 and gamma1
#**************************************
getPhi <- function(UFull, B, Phi, S){
  
  gamma0 <- Phi[1]
  gamma1 <- Phi[2]
  
  TT = ncol(UFull)
  
  tot_nmrt_gamma0 <- 0
  tot_dnmt_gamma0 <- 0
  
  tot_nmrt_gamma1 <- 0
  tot_dnmt_gamma1 <- 0
  
  ##coordinate-wise update gamma0 and gamma1
  #gamma0
  tot_nmrt_gamma0 <- sum(S[,-1] * (UFull[,-1] - gamma1*UFull[,-TT]) * B[,-1])
  tot_dnmt_gamma0 <- sum(S[,-1] * (B[,-1]^2))
  new_gamma0 <- tot_nmrt_gamma0/tot_dnmt_gamma0
  gamma0 <- new_gamma0
  
  #gamma1
  tot_nmrt_gamma1 <- sum(S[,-1] * (UFull[,-1] - gamma0 * B[,-1]) * UFull[,-TT])
  tot_dnmt_gamma1 <- sum(S[,-1] * (UFull[,-TT])^2)
  new_gamma1 <- tot_nmrt_gamma1/tot_dnmt_gamma1
  gamma1 <- new_gamma1
  
  retval = c(gamma0, gamma1)
  names(retval) = c("gamma0", "gamma1")
  return(retval)	
}

#***********************************************************
#******************************
# estimate parameters beta,
# residual and number
#******************************
getRes_Num <- function(data, UFilled){
  new.data = getIndHouse(data, UFilled)
  house.model = getBeta(new.data)
  fit.value = house.model$value
  data$residual = data$resp - fit.value
  
  group_period_residual <- data %>%
    dplyr::select(residual, CommunityID, TradingYear) %>%
    group_by(CommunityID, TradingYear) %>%
    summarise(sum_residual = sum(residual), nums = n())
  
  group_period_residual = data.frame(group_period_residual)
  
  H = UFilled
  Nums = UFilled
  
  group = rownames(UFilled)
  period = colnames(UFilled)
  
  
  is = match(group_period_residual$CommunityID, group)
  ts = match(group_period_residual$TradingYear, period)
  hs = group_period_residual$sum_residual
  ns = group_period_residual$nums
  idxs = (ts-1)*length(group) + is
  H[idxs] = hs
  Nums[idxs] = ns
  
  retval = list(Res = H, Num = Nums, house.model = house.model)	
  return(retval)
}

getIndHouse <- function(data, Uhat){
  #input: data/original dataframe
  #output: new.data/dataframe with the unit house value reduced from neighborhood's value
  
  new.data = data
  group = rownames(Uhat)
  period = colnames(Uhat)
  
  rows = match(new.data$CommunityID, group)
  cols = match(new.data$TradingYear, period)
  
  U = mapply(function(x,y){
    return(Uhat[x, y])
  }, rows, cols)
  
  new.data$resp = new.data$resp - U
  
  return(new.data)
}

getBeta <- function(new.data){
  
  ##all model
  theta.model <- lm(resp ~ BedroomNum + LivingroomNum + Size +
                      Orientation + FloorOfHouse + Age + BuildingType + FloorOfBuilding +
                      District + FloorAreaRatio + LandscapingRatio + HousingType + BuildingNum +
                      HousingNum + SubwayLineInfo + SchoolID + NeighborBusiness, data = new.data)
  
  ##without location
  # theta.model <- lm(resp ~ BedroomNum + LivingroomNum + Size +
  #                     Orientation + FloorOfHouse + Age + BuildingType + FloorOfBuilding +
  #                     HousingType + HousingNum, data = new.data)
  
  ##without individual
  # theta.model <- lm(resp ~ District + SubwayLineInfo + SchoolID + NeighborBusiness +
  #                   BuildingNum + FloorAreaRatio + LandscapingRatio, data = new.data)
  
  
  beta <- theta.model$coefficients
  fitted_value <- theta.model$fitted.values
  
  rslt <- list(beta = beta, value = fitted_value, model = theta.model)
  return(rslt)
}
#*******************************************************************************************





#************************
# update UFull using
# coordinate descent
#************************
updateUFull_B <- function(UFull, A, B, S, S_tilde, 
                        Phi, H, Num, xi0, xi1){
  

  gamma0 <- Phi[1]
  gamma1 <- Phi[2]

  M <- nrow(UFull)
  TT <- ncol(UFull)
  
  idx_Full <- which(S == 1 | S_tilde == 1)
  
  for (idx in idx_Full){
    ts <- ceiling(idx/M)
    i <- idx - (ts - 1) * M
    print(ts)
    
    if(S[i,ts] == 1){
      residual <- H[i,ts]
      num <- Num[i,ts]
      
      if(ts == 1){
          dmnt <- S[i,ts]*(num+xi0+xi1) + 
                  S[i,ts+1]*xi0*(gamma1^2) + 
                  xi0*sum(S[,ts] * ((A[[ts]][,i])^2))
        
          nmrt <- S[i,ts]*(residual + xi0*B[i,ts]) +
                  S[i,ts+1]*xi0*gamma1*(UFull[i,ts+1] - gamma0*B[i,ts+1]) +
                  xi0*sum(S[,ts]*A[[ts]][,i] * (UFull[,ts] - B[,ts] + A[[ts]][,i]*UFull[i,ts]))
          
      }else{
        if(ts == TT){
          dmnt <- S[i,ts]*(num+xi0+xi1) +
            xi0*sum(S[,ts]*(gamma0*A[[ts]][,i])^2)
          
          nmrt <- S[i,ts]*(residual + xi0*(gamma0*B[i,ts] + gamma1*UFull[i,ts-1])) +
            xi0*sum(S[,ts]*gamma0*A[[ts]][,i] * 
                      (UFull[,ts] - gamma0*(B[,ts] - A[[ts]][,i]*UFull[i,ts]) - gamma1*UFull[,ts-1]))
        }else{
          dmnt <- S[i,ts]*(num+xi0+xi1) +
            S[i,ts+1]*xi0*(gamma1)^2 +
            xi0*sum(S[,ts]*(gamma0*A[[ts]][,i])^2)
          
          nmrt <- S[i,ts]*(residual + xi0*(gamma0*B[i,ts] + gamma1*UFull[i,ts-1])) +
            S[i,ts+1]*xi0*gamma1*(UFull[i,ts+1] - gamma0*B[i,ts+1]) +
            xi0*sum(S[,ts]*gamma0*A[[ts]][,i] * 
                      (UFull[,ts] - gamma0*(B[,ts] - A[[ts]][,i]*UFull[i,ts]) - gamma1*UFull[,ts-1]))
        }
        
      }
      
         UFull[i,ts] <- nmrt/dmnt
        
    }else{
         UFull[i, ts] <- gamma0*B[i,ts] + gamma1*UFull[i,ts-1]
    }
    
    B[,ts] <- A[[ts]] %*% UFull[,ts]
  }
  
  return(list(U = UFull, B = B))
}

#************************
# update UFull using
# coordinate descent
# speedup
#************************
updateUFull_B_Speedup <- function(UFull, A, B, S, S_tilde, 
                          Phi, H, Num, xi0, xi1){
  
  #browser()
  gamma0 <- Phi[1]
  gamma1 <- Phi[2]
  
  M <- nrow(UFull)
  TT <- ncol(UFull)
  
  
  
  for(ts in 1:TT){
    #print(ts)
    
    idx_Filled <- which(S[,ts] == 1)
    idx_Missing <- which(S_tilde[,ts] == 1)
    
    
    num <- Num[idx_Filled,ts]
    residual <- H[idx_Filled,ts]
    
    #updata UFilled
    if(ts == 1){
      dmnt <- S[idx_Filled,ts]*(num+xi0+xi1) + 
              S[idx_Filled,ts+1]*xi0*(gamma1^2) + 
              xi0*colSums(S[,ts] * ((A[[ts]][,idx_Filled])^2))
      
      nmrt <- S[idx_Filled,ts]*(residual + xi0*B[idx_Filled,ts]) +
              S[idx_Filled,ts+1]*xi0*gamma1*(UFull[idx_Filled,ts+1] - gamma0*B[idx_Filled,ts+1]) +
              xi0*colSums( S[,ts]*A[[ts]][,idx_Filled] * (UFull[,ts] - B[,ts] + t(t(A[[ts]][,idx_Filled])*UFull[idx_Filled,ts])) )

    }else if (ts == TT){
      
      dmnt <- S[idx_Filled,ts]*(num+xi0+xi1) +
              xi0*colSums(S[,ts]*(gamma0*A[[ts]][,idx_Filled])^2)
      
      nmrt <- S[idx_Filled,ts]*(residual + xi0*(gamma0*B[idx_Filled,ts] + gamma1*UFull[idx_Filled,ts-1])) +
              xi0*gamma0*colSums( S[,ts]*A[[ts]][,idx_Filled] * 
              (UFull[,ts] - gamma0*(B[,ts] - t(t(A[[ts]][,idx_Filled])*UFull[idx_Filled,ts])) -  gamma1*UFull[,ts-1]))
      
    }else{
      dmnt <- S[idx_Filled,ts]*(num+xi0+xi1) +
              S[idx_Filled,ts+1]*xi0*(gamma1)^2 +
              xi0*colSums(S[,ts]*(gamma0*A[[ts]][,idx_Filled])^2)
      
      
      nmrt <- S[idx_Filled,ts]*(residual + xi0*(gamma0*B[idx_Filled,ts] + gamma1*UFull[idx_Filled,ts-1])) +
              S[idx_Filled,ts+1]*xi0*gamma1*(UFull[idx_Filled,ts+1] - gamma0*B[idx_Filled,ts+1]) +
              xi0*gamma0*colSums( S[,ts]*A[[ts]][,idx_Filled] * 
              (UFull[,ts] - gamma0*(B[,ts] - t(t(A[[ts]][,idx_Filled])*UFull[idx_Filled,ts])) -  gamma1*UFull[,ts-1]))
      
    }
    
    UFull[idx_Filled,ts] <- nmrt/dmnt
    
    #update UMissing
    if(length(idx_Missing) > 0){UFull[idx_Missing, ts] <- gamma0*B[idx_Missing,ts] + gamma1*UFull[idx_Missing,ts-1]}
    
    #update matrix B
    B[,ts] <- A[[ts]] %*% UFull[,ts]
    
  }
  
  return(list(U = UFull, B = B))
}



#******************
# extract UFilled 
# from UFull
#******************
getUFilled <- function(UFull, S){
  idx_Filled <- which(S == 1)
  M = nrow(UFull)
  TT = ncol(UFull)
  
  UFilled = matrix(0, M, TT)
  UFilled[idx_Filled] <- UFull[idx_Filled]
  
  rownames(UFilled) <- rownames(UFull)
  colnames(UFilled) <- colnames(UFull)
  
  return(UFilled)
}


#**********************************************************************************************
# calculate objective function
#**********************************************************************************************
cal_loglik <- function(Phi, UFull, S, B, hmodel, data, xi0, xi1){
  
  gamma0 <- Phi[1]
  gamma1 <- Phi[2]
  
  #J1
  fit.value <- hmodel$value
  group <- rownames(UFull)
  period <- colnames(UFull)
  TT <- length(period)
  rows <- match(data$CommunityID, group)
  cols <- match(data$TradingYear, period)
  
  U <- mapply(function(x,y){
    return(UFull[x, y])
  }, rows, cols)
  temp <- data$resp - fit.value - U
  J1<- sum(temp^2)
  
  #J2
  J2 <- sum(S[,1] * (UFull[,1] - B[,1])^2) +
        sum(S[,-1] * (UFull[,-1] - gamma0*B[,-1] - gamma1*UFull[,-TT])^2)

  ##J3
  J3 <- sum(S * UFull^2)
  
  J = 1/2*(J1 + xi0*J2 + xi1*J3)

  
  retval <- c(J,J1, J2, J3)
  names(retval) = c('J','J1','J2','J3')
  return(retval)
}



#********************************************************************************


#**********************************
# predict missing values
#**********************************
predictUMissing <- function(UFilled, S_tilde, B, Phi){
  #input: UFilled/matrix 
  #input: Phi/initial parameters
  #output: initilization and prediction of the neighborhoods value with the support of filled value
  gamma0 <- Phi[1]
  gamma1 <- Phi[2]
  
  UFull <- UFilled #Missing value will be imputed by prediction
  idx_missing <- which(S_tilde == 1)
  
  M <- nrow(UFilled)
  TT <- ncol(UFilled)
  
  for (idx in idx_missing){
    ts <- ceiling(idx/M)
    i <- idx - (ts - 1) * M
    
    UFull[i, ts] <- gamma0*B[i, ts] + gamma1*UFull[i,ts-1]
  }
  
  return(UFull)
}


getTestU <- function(newdata, train.U, Phi, w_period, q = 1, nb, nb_period){
	
	gamma0 = Phi[1]
	gamma1= Phi[2]
	

	test.group = as.character(sort(unique(newdata$CommunityID)))
	test.period = as.character(sort(unique(newdata$TradingYear)))
	
	#browser()
	
	train.group = rownames(train.U)
	train.period = colnames(train.U)
	
	
	
	idx.old.group = which(test.group %in% train.group)
	idx.new.group = which(!(test.group %in% train.group))
	
	old.group = test.group[idx.old.group]
	new.group = test.group[idx.new.group]
	
	##generate new UFilled
	train.test.U = matrix(NA, length(train.group)+length(new.group), 1+length(test.period))
	train.test.U[1:nrow(train.U), 1] = train.U[, ncol(train.U)]
	
	group = c(train.group, new.group)
	period = c(train.period[length(train.period)], test.period)
	
	
	
	rownames(train.test.U) = group
	colnames(train.test.U) = period
	
	
	##generate new indicator matrix
	indMatrix = matrix(0, length(train.group)+length(new.group), 1+length(test.period))
	indMatrix[1:nrow(train.U), 1] = 1
	
	
	
	rownames(indMatrix) = group
	colnames(indMatrix) = period
	
	
	## for old groups
	final_w_period = w_period[[length(w_period)]]
	#browser()
	w_period_old_group = rep(list(final_w_period), 1+length(test.period))
	names(w_period_old_group) = c(train.period[length(train.period)],test.period)
	
	
	
	RMissing = geneRecMissingMatrix(indMatrix)
	UFull_old_group = predictUMissing(UFilled = train.test.U[1:nrow(train.U), ], RMissing[1:nrow(train.U), ], 
	                                  w_period = w_period_old_group, Phi)
	
	train.test.U[1:nrow(train.U), ] = UFull_old_group
	
	
	## for new group, we only leverage old groups to estiamte them, so weights keep same
	final_nb_period = nb_period[length(nb_period)]
	nb_new_group = nb[new.group]
	

	
	w_new_group <- lapply(new.group, function(x){
		nb_x = nb[[x]]
		old_group_x = nb_x[names(nb_x) %in% old.group]
		
		if (length(old_group_x) > 0){
			w = exp(-q*(old_group_x)^2)
			w = w/sum(w)
		}else{
			w = NULL  #to be revisted later
		}
		return(w)
	})
	
	names(w_new_group) = new.group
	
	
	for(t in 2:ncol(train.test.U)){
		for (i in (nrow(train.U)+1):nrow(train.test.U)){		
		  
		  #browser()
			
			temp = w_new_group[[group[i]]]
			
			if(is.null(temp)){
				if(is.na(train.test.U[i,t-1])){
					train.test.U[i,t] = mean(train.test.U[,t-1], na.rm = T)     #to be revisited later
				}else{
					train.test.U[i,t] = train.test.U[i,t-1]
				}
			}else{
				i_prime = match(names(temp),group)
				lp = sum(train.test.U[i_prime, t-1]*temp)
				
				if(is.na(train.test.U[i,t-1])){
				  train.test.U[i,t] = lp
				}else{
				  train.test.U[i,t] = gamma0*lp + gamma1*train.test.U[i,t-1]
				}
			}
		}
	}
	
	return(train.test.U)
}


#********************
# based on trained U
#********************
getTestU2 <- function(newdata, train.U, Phi, p = 1, q = 2, nb, K){
  
  gamma0 = Phi[1]
  gamma1= Phi[2]
  
  test.group = as.character(sort(unique(newdata$CommunityID)))
  test.period = as.character(sort(unique(newdata$TradingYear)))
  
  #browser()
  
  train.group = rownames(train.U)
  train.period = colnames(train.U)
  
  idx.old.group = which(test.group %in% train.group)
  idx.new.group = which(!(test.group %in% train.group))
  
  old.group = test.group[idx.old.group]
  new.group = test.group[idx.new.group]
  
  
  ##generate new UFilled
  train.test.U = matrix(0, length(train.group)+length(new.group), 1+length(test.period))
  train.test.U[1:nrow(train.U), 1] = train.U[, ncol(train.U)]
  
  group = c(train.group, new.group)
  period = c(train.period[length(train.period)], test.period)
  
  rownames(train.test.U) = group
  colnames(train.test.U) = period
  
  
  
  ##old group
  train.test.U[1:length(train.group), 2:ncol(train.test.U)] <- train.test.U[1:length(train.group), 1]
  
  ##new group
  
  w_new_group <- lapply(new.group, function(x){
    nb_x = nb[[x]]
    old_group_x = nb_x[names(nb_x) %in% old.group]
    
    idx = order(old_group_x, decreasing = T)
    topK = min(length(idx), K)
    old_group_x_topK = old_group_x[1:topK]
    
    if (length(old_group_x) > 0){
      w = exp(-q*(old_group_x)^p)
      w = w/sum(w)
    }else{
      w = NULL  #to be revisted later
    }
    return(w)
  })
  
  names(w_new_group) = new.group
  
  for(t in 2:ncol(train.test.U)){
    for (i in (nrow(train.U)+1):nrow(train.test.U)){		
      
      temp = w_new_group[[group[i]]]
      
      if(is.null(temp)){
          train.test.U[i,t] = mean(train.test.U[,1], na.rm = T)     #to be revisited later
      }else{
           j = match(names(temp),group)
           B_its = sum(train.test.U[j, 1]*temp)

           train.test.U[i,t] = B_its
        }
      }
  }
  
  return(train.test.U)
}
