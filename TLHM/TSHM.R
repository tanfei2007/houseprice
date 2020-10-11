
TSHM <- function(data, #data
				   group, #colname of group
				   period, #names of all periods
				   nb, #neighborhood information(with distance information)
				   p =1, 
				   q = 1, #the kernel hyper-parameter
				   maxIter = 500, #maximum iterations number  
				   K = 10,
				   xi0 = 1,
				   xi1 = 1
){
  
  #browser()
  #record objective function
  Js <- matrix(0,maxIter+1,4)
  
	#generate a few utility matrix
	S <- geneS(data, group, period)
	S_tilde <- geneS_tilde(S)
	#if(!file.exists('D.RDATA')){
	  D <- generate_D(nb, S)
	#  save(file = 'D.RDATA', D)
	#}else{
	#  load('D.RDATA')
	#}
	
	A <- generate_A(D, S, K, p, q)
	#browser()

	#parameter initilization
	Phi = initPhi()
	
	UFilled = initUFilled(data, S)
	B = initB(A, UFilled)
	
	res_num_hmodel = getRes_Num(data, UFilled)
	H = res_num_hmodel$Res
	Num = res_num_hmodel$Num
	Theta = res_num_hmodel$house.model$beta
	
	UMissing = initUMissing(UFilled, S_tilde, B, Phi)
	UFull = UFilled + UMissing
	
	Phi.trace = Phi
	Theta.trace = Theta
	
  #calculate objective function
  J <- cal_loglik(Phi, UFull, S, B, res_num_hmodel$house.model, data, xi0, xi1)
	Js[1,1:4] <- J
	
	
	#iteration
	for(run in 1:maxIter){
		print(run)
	  print(Phi)
	  print(J)
	  
		Phi <- getPhi(UFull, B, Phi, S)
		
		UFull_B <- updateUFull_B_Speedup(UFull, A, B, S, S_tilde, Phi, H, Num, xi0, xi1)
		UFull <- UFull_B$U
		B <- UFull_B$B
		
		UFilled = getUFilled(UFull, S)
		res_num_hmodel = getRes_Num(data, UFilled)
		H = res_num_hmodel$Res
		Theta = res_num_hmodel$house.model$beta
		
		Phi.trace = cbind(Phi.trace, Phi)
		Theta.trace = cbind(Theta.trace, Theta)
		
  
		J <- cal_loglik(Phi, UFull, S, B, res_num_hmodel$house.model, data, xi0, xi1)
		Js[run+1,1:4] <- J
		
		##check for convergence
		stop.Theta <- max(abs(Theta.trace[,ncol(Theta.trace)]-Theta.trace[,ncol(Theta.trace)-1])/Theta.trace[,ncol(Theta.trace)-1])
	  stop.Phi <- max(abs(Phi.trace[,ncol(Phi.trace)]-Phi.trace[,ncol(Phi.trace)-1])/Phi.trace[,ncol(Phi.trace)-1])

		this.Theta <- 0.0001
		this.Phi <- 0.0001
		
		if(stop.Theta < this.Theta && stop.Phi < this.Phi)
		  break
	}
	
	
	##Theta and Phi rename
	Theta.trace <- Theta.trace[, -1]
	Phi.trace <- Phi.trace[, -1]
	colnames(Theta.trace) <- paste('run', 1:ncol(Theta.trace), sep = '')
	colnames(Phi.trace) <- paste('run', 1:ncol(Phi.trace), sep = '')
	rownames(Phi.trace) <- c('gamma0', 'gamma1')
	
	return(list(U = UFull, Phi = Phi, Phi.trace = Phi.trace, Theta = Theta, Theta.trace = Theta.trace, hmodel = res_num_hmodel$house.model,
	            A = A, B = B, ObFuns = Js))
	}
