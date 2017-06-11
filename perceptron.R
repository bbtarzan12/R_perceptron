x1 <- runif(30,-1,1)
x2 <- runif(30,-1,1)

x <- cbind(x1,x2)
Y <- ifelse(x2>0.5+x1, +1, -1)

calculate_distance = function(x,w,b){
  sum(x*w) + b
}

linear_classifier = function(x,w,b){
  distances = apply(x, 1, calculate_distance, w, b)
  return(ifelse(distances < 0, -1, +1))
}

second_norm = function(x) {sqrt(sum(x*x))}

perceptron = function(x, y, learning_rate = 1){
  w = vector(length = ncol(x))
  b = 0
  k = 0
  
  R = max(apply(x, 1, second_norm))
  
  incorrect = TRUE
  
  plot(x, cex=0.2)
  
  while(incorrect){
    incorrect = FALSE
    
    yc <- linear_classifier(x,w,b)
    
    for(i in 1:nrow(x)){
      if(y[i] != yc[i]){
        w <- w + learning_rate * y[i] * x[i,]
        b <- b + learning_rate * y[i] * R^2
        k <- k + 1
        
      # if(k%%5 == 0){
      #   intercept <- -b / w[[2]]
      #   slope <- -w[[1]] / w[[2]]
      #   abline(intercept,slope,col="red")
      #   cat("Iteration # ", k, "\n")
      #   cat("Press [enter] to continue")
      #   line <- readline()
      # }
      incorrect = TRUE
      }
    }
  }
  y <- linear_classifier(x,p$w,p$b)
  plot(x, cex=0.2)
  points(subset(x,y==1),col="black",pch="+",cex=2)
  points(subset(x,y==-1),col="red",pch="-",cex=2)
  intercept <- -p$b / p$w[[2]]
  slope <- -p$w[[1]] / p$w[[2]]
  abline(intercept,slope,col="green")
  
  
  s = second_norm(w)
  return(list(w=w/s,b=b/s,updates=k))
}

p <- perceptron(x, Y)
