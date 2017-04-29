
 #library(calibrate)

ants_cycle <- function(app,m,n,h,t,alpha,beta){
  for(i in 1:m){
    mh <- h
    for(j in 1:(n-1)){
      c <- app[i,j];
      mh[,c] <- 0;
      temp <- (t[c,]^beta)*(mh[c,]^alpha);
      s <- sum(temp)
      p <- (1/s)*temp
      r <- runif(1)
      s <- 0
      for(k in 1:n){
        s <- s+p[k]
        if (r <= s) {
          app[i,j+1] <- k
          break
        }
      }
    }
  }
  at <- app
  return (at)
}



ants_information <- function(x,y){
  iter <- 20; #number of cycles
  m <- 30; #number of ants

  n <- length(x); #number of nodes
  d <- matrix(0,nrow = n, ncol = n); #link lenght matrix
  #generating link length matrix
  for(i in 1:n){
    for(j in 1:n){
      d[i,j] <- sqrt((x[i]-x[j])^2+(y[i]-y[j])^2);
    }
  }
  e <- 0.1; #evaporation coefficient
  alpha <- 1; #order of effect of ants' sight
  beta <- 5; #order of trace's effect
  h <- matrix(nrow =n, ncol = n); #sight matrix
  #generating sight matrix
  for(i in 1:n){
    for(j in 1:n){
      if((d[i,j] == 0)){
        h[i,j] <- 0;
      }
      else{
        h[i,j] <- 1/d[i,j];
      }
    }
  }
  t <- 0.0001*matrix(1,n,n);
  el <- 0.96; #coefficient of common cost elimination
  return(list(x,y,d,t,h,iter,alpha,beta,e,m,n,el))
}
ants_primaryplacing <- function(m,n){
  app = matrix(0,nrow = m, ncol = n);
  # for(i in 1:m){
  # 	app[i,1] = round(1+runif(1)*(n-1))
  # }
  app[,1] = round(1+runif(m)*(n-1))
  return (app)
}

ants_cost <- function(m,n,d,at,el){
  #cat(n)
  f <- c()
  for(i in 1:m){
    s <- 0
    for(j in 1:n){
      s <- s + d[at[i,j], at[i,j+1]]
    }
    f[i] <- s
  }
  cost <- f
  f <- f-el*min(f); #elimination of common cost
  return(list(cost,f))
}
ants_tranceupdating <- function(m,n,t,at,f,e){
  for(i in 1:m){
    for(j in 1:n){
      dt <- 1/f[i]
      t[at[i,j], at[i,j+1]] = (1-e)*t[at[i,j], at[i,j+1]]+dt; #updating t
    }
  }
  return(t)
}

# plot<-function(...){
#   graphics::plot.new()
#   graphics::plot(...)
# }

main <- function(){
  df <- read.table("http://www.math.uwaterloo.ca/tsp/world/wi29.tsp",
                 header = FALSE, skip=7, nrows=29)
  # df <- read.table("http://www.math.uwaterloo.ca/tsp/world/dj38.tsp",
  #                header = FALSE, skip=10, nrows=38)
  # df <- read.table("http://www.math.uwaterloo.ca/tsp/world/zi929.tsp",
  #                header = FALSE, skip=7, nrows=929)

  debug <- TRUE
  if(debug)
    print("Done reading")
  x <- df[2]
  x <- x[,1]
  y <- df[3]
  y <- y[,1]
  # y <- c(2,4,6,-1,-2,0.5,0,3.7,1.8,1,0,4,3,2);
   # x <- c(8,0,-1,2,4,6,3,10,2.5,-5,7,9,11,13);
   # y <- c(2,4,6,-1,-2,0.5,0,3.7,1.8,1,0,4,3,2);
  ants_information <- ants_information(x,y);
  #x <- ants_information[[1]]; y <- ants_information[[2]];
  d <- ants_information[[3]];
  t <- ants_information[[4]]; h <- ants_information[[5]]; iter<-ants_information[[6]];
  alpha<-ants_information[[7]];beta<-ants_information[[8]];e<-ants_information[[9]]
  m <- ants_information[[10]]; n <- ants_information[[11]];el<-ants_information[[12]]
  #iter<- 10;
  besttour = matrix(nrow = iter, ncol =n+1)
  iteration <- c()
  costoa <- c()
  mincost <- c()
  flag = 0
  for(i in 1:iter){
    startIterTime <- Sys.time()
    app <- ants_primaryplacing(m,n); #randomly place ants
    # if(flag ==1){
    # 	for(foo in app[,1]){
    # 		if(runif(1) > 0.5){
    # 			app[foo,1] <- at[foo,1]
    # 		}
    # 	}
    # }
    # 	flag = 1
    at <- ants_cycle(app,m,n,h,t,alpha,beta); #ants make their moves
    at <- cbind(at, at[,1]) #append first col back into matrix for new last col
    ants_cost <- ants_cost(m,n,d,at,el)#calculat cost of ants moves
    cost <- ants_cost[[1]];f <- ants_cost[[2]];
    t <- ants_tranceupdating(m,n,t,at,f,e)
    costoa[i] <- mean(cost);
    mincost[i] <- min(cost);
    number <- which.min(cost); #index of where is min
    besttour[i,] <- at[number,]
    iteration[i] <- i
    endIterTime <- Sys.time()
    if(debug)
      print(sprintf("%d:   %f  took: %s   best so far:%f  found at iteration:%d ", i,mincost[i],endIterTime- startIterTime, min(mincost), which.min(mincost)))

  }
  #windows()
  par(mfrow=c(2,1))
  # title <- "average of cost (distance) versus number of cycles"
  # plot(iteration,costoa, type="l", main=title, xlab = "iteration", ylab ="distance")
  k <- min(mincost);
  l <- which.min(mincost); #index of where is min
  X <- Y <- c();

  for(i in 1:(n+1)){
    X[i] <- x[besttour[l,i]]
    Y[i] <- y[besttour[l,i]]
    #feed ant path into r
    #cat(X[i], "," ,Y[i], "\n")
    #feed regular path into r
    # if(i < n+1)
    # 	cat(x[i], "," ,y[i], "\n")
    # else
    # 	cat(x[1], "," ,y[1], "\n")
  }
  #  print(X)
  # print(Y)
  # title2 <- sprintf("optimal course by length of %f", k)
  # plot(X,Y, main=title2, type="o", pch=22, lty=2.5, col="red")
  # coordinates <- c()
  # for(i in 1: length(X)){
  # 	coordinates[i] <- sprintf("(%f,%f)",X[i], Y[i])
  # }
  # textxy(X,Y,coordinates,m=c(0,0))
  stringPoint <-

  s <- paste(X,Y)
  s <- paste(toString(s),k)
  # for(l in length(X)){
  # 	s <- paste(s,X,Y, sep=" ")
  # }
  if(k < best){
    # tmp <- (k < best)
    # cat(best, " vs ", k, " result ", tmp)
    # print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    best <<- k
    #dev.set(dev.prev(which = dev.cur()))
    dev.off()
    title <- "average of cost (distance) versus number of cycles"
    plot(iteration,costoa, type="l", main=title, xlab = "iteration", ylab ="distance")
    title2 <- sprintf("optimal course by length of %f", k)
    plot(X,Y, main=title2, type="o", pch=22, lty=2.5, col="red")

  }
  #graphics.off()
  return(s)
}
sA <- c()
string <- ""
isAppend <- FALSE
best <- Inf
#dev.new()
for(i in 1:5){
  sA <- main()
  write(toString(sA),file="myData.txt",append=isAppend)
  isAppend <- TRUE
  write(proc.time(),file="myData.txt",append=isAppend)


}
rm(list=ls())

# main()
 #locator(1)
# x_last <- runif(1)
# for(i in 1:15){
# x_now <- runif(1)
# print(abs(x_last-x_now))
# x_last <- x_now
# }

