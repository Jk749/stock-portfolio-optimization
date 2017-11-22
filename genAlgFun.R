geneticAlgo<- function(population,fitnessFun,mutate, 
                       crossOver, mutProb,elite, maxiterations){
  
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize=nrow(population)
  topElite=round(elite*origPopSize,0)
  fitN=apply(population,1,fitnessFun)
  population=data.frame(population,fitN)
  population=population[sort.list(population[,ncol(population)],decreasing = T), ]
  maxi=population[1,ncol(population)]
  wts = population[1,-c(ncol(population))]
  # Main loop
  
  for (i in 1:maxiterations) {
    population=population[1:topElite,]
    population=population[,-c(ncol(population))]
    mut=mutProb/i      #need to check on changing mutate prob as iterations progress - ??
    
    # Add mutated and bred forms of the winners
    while (nrow(population)<origPopSize) {
      # Mutation
      if (runif(1,0,1)<mut) {
        c=sample(1:topElite,1)
        population[nrow(population)+1,]=mutate(population[c,])
      }
      
      # Crossover
      else {
        c1=sample(1:topElite,1)
        c2=sample(1:topElite,1)
        population[nrow(population)+1,]=data.frame(crossOver(population[c1,], population[c2,]))
      }
      
    }
    
    population[,ncol(population)+1]=apply(population,1,fitnessFun)
    population=population[sort.list(population[,ncol(population)],decreasing = T), ]
    
    # Print current best score
    
    cat("best fitness score in iteration", i, "=", population[1,ncol(population)], "\n")
    
    #store the previous best score and compare it with current score
    #find max best score as iterations progress and then output the best value,
    #so storage of max best value is pivotal in every iteration and
    #also no termination in middle of the loop
    #Convergence
    if(population[1,ncol(population)]>maxi){
      maxi = population[1,ncol(population)]
      wts = population[1,-c(ncol(population))]
    }
    
    #returns the best solution   
    
  }
  return(population[1,])
}

