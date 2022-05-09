

L = data.frame(letter=letters[],value=c(1:26))
my_vec=c("e" ,"v", "a", "2", "3", "3", "c", 
         "h", "a", "r", "i", "z", "a", "n", "o", "u")


calc_pass <- function (name, last_name, digits){
  x <- 0  
  a <- name
  b <- last_name
  vec <- c(a,b) #add them in a vector
  x <- sum(as.numeric(digits))
  i<-1
  #protimao na xrisimopoiisw while giati to for loop einai arketa argo stin R
  while (i <= length(vec)){
    x <- x + L[which(L == vec[i]),2]
    i <- i + 1
  }
  return(x)
}

x <-  calc_pass(my_vec[1:3],my_vec[7:16],my_vec[4:6])


# this function returns if a number is prime or not
is_prime <- function (n){
  if ( n <= 1){
    return( FALSE )
  } else if ( n <= 3){
      return( TRUE )
  } else if ( n %% 2 == 0 | n %% 3 == 0){
      return( FALSE )
  }
  i <- 5
  while ( i*i <= n ){
    if ( n %% i == 0 | n %% (i+2) == 0){
      return(FALSE)
    }
    i <- i + 6
  }
  return (TRUE)
}


SumOfPrimeDivisors <- function (n){
  sum <- 0
  root_n <- as.integer(sqrt(n))
  for ( i in 1:root_n){
    if ( n %% i == 0){
      if (i == as.integer(n/i) & is_prime(i)){
        sum <- sum + i
      } else {
          if (is_prime(i)){
             sum <- sum + i
          }
          if (is_prime(as.integer(n/i))){
            sum <- sum + as.integer(n/i)
          }
      } 
    }
  } 
  print(i)
  return(sum)
}

y <- SumOfPrimeDivisors( 10 * x +1000 )
set.seed(y)
library(ggplot2)

#EROTIMA 1 
X = rlnorm(500) #sample 
dat = data.frame(x=X)
ggplot(dat) +
  geom_histogram(aes(x=X,y=..density..),alpha=0.4, binwidth = 0.1, col="red") +
  geom_function(fun=dlnorm,col="black",size=1) +
  xlim(c(-0.1,10)) +
  theme_minimal()

##na dw an xreiazetai na epanalavw to set seed
#EROTIMA 2
X_sample1 <- X[1:20]
ks.test(X_sample1, "plnorm",exact = TRUE)

d <- 20/5
breaks_cdf <- qlnorm(0:d / d)
h1 <- hist(X_sample1, breaks_cdf, plot = FALSE)
chisq.test(h1$counts, simulate.p.value = TRUE)


X_sample2 <- X[1:100]
ks.test(X_sample2, "plnorm")

d <- 100/10
breaks_cdf <- qlnorm(0:d / d)
h2 <- hist(X_sample1, breaks_cdf, plot = FALSE)
chisq.test(h2$counts, simulate.p.value = TRUE)


ks.test(X, "plnorm")

d <- 500/50
breaks_cdf <- qlnorm(0:d / d)
h3 <- hist(X, breaks_cdf, plot = FALSE)
chisq.test(h3$counts, simulate.p.value = TRUE)




#EROTIMA 3
testLogNormal <- function(N, n, alpha){
  set.seed(y)
  fails_ks <- 0
  fails_chisq <- 0 
  d <- floor(n / 10)
  if ( n < 100){
    d = floor( n / 5)
  }
  breaks_cdf <- qlnorm( 0:d / d)
  for ( i in 1:N){
    Ysample <- rlnorm(n)
    decision_ks <- ks.test( Ysample, "plnorm")$p.value
     if (decision_ks < alpha){
      fails_ks = fails_ks + 1
     }
    h <- hist(Ysample, breaks_cdf, plot=FALSE)
    decision_chisq <- chisq.test(h$counts, simulate.p.value = FALSE)$p.value
    if (decision_chisq < alpha){
      fails_chisq = fails_chisq + 1
    }
  }
  return(c(fails_ks / N, fails_chisq / N))
}

#EROTIMA 4

U <- plnorm(X)
V <- runif(100)
ks.test(U,V)
t.test(U,V)

#EROTIMA 5

Y <- qlnorm(V)
ks.test(Y[1:20], "plnorm")

d = 20 / 5
breaks_cdf = qlnorm(0:d / d)
h = hist(Y[1:20], breaks_cdf, plot = FALSE)
chisq.test(h$counts)



ks.test(Y, "plnorm")

d = 100 / 10
breaks_cdf = qlnorm(0:d / d)
h = hist(Y, breaks_cdf, plot = FALSE)
chisq.test(h$counts)


