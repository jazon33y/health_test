#################
# Run only once
#################
# parameter.bernoulli <- function(x) {
#   n <- length(x);k <- sum(x);a = k;b = n-k+1;c = k+1;d = n-k
#   if (k == 0) a = .0001
#   if (n-k+1 == 0) b = .0001
#   if (n-k == 0) d = .0001
#   return(list(qbeta(1:1000/1000,a, b), qbeta(1:1000/1000,c, d)))
# }
# 
# breadth<-function(x){
#   probs = 0:length(x[[1]])/length(x[[1]])
#   s = 1/length(x[[1]])
#   x_quants = quantile(x[[1]],probs=probs,type=1,names=FALSE)
#   y_quants = quantile(x[[2]],probs=probs,type=1,names=FALSE)
#   return(sum(s*(abs(x_quants-y_quants))))
# }
# 
# data = list()
# for (i in 1:9999){#150){
#   a = rbinom(n=i, size=1,prob=.5)
#   name <- paste('item:',i,sep='')
#   data[[name]] = a
# }
# 
# breadths=c(1)
# for (i in data){
#   b = breadth(parameter.bernoulli(i))
#   breadths=c(breadths,b)
#   #print(b)
# }
#################


p_0 = 0.0001; k_0 = 0.0001; s_0 = 0.0001; t_0 = 0.0001; # reduces excessively puffy tails
p_1 = 0.9999; k_1 = 0.9999; s_1 = 0.9999; t_1 = 0.9999; # reduces excessively puffy tails
 
PPV = function(p,s,t) ifelse(p==0,(1/(1+((1/p_0-1)*(1-t))/s)),ifelse(s==0,(1/(1+((1/p-1)*(1-t))/s_0)),(1/(1+((1/p-1)*(1-t))/s))))
NPV = function(p,s,t) ifelse(p==0,1/(1+(1-s)/(t*(1/p_0-1))),ifelse(t==0,1/(1+(1-s)/(t_0*(1/p-1))),1/(1+(1-s)/(t*(1/p-1))) ))

balch <- function(k, n,many=1000) {
  if (n < k) stop("The value of n (", n, ") cannot be smaller than k (",k,")")
  uu = function() qbeta(1:many/many, k, n - k + 1)
  dd = function() qbeta(1:many/many, k + 1, n - k)
  if ((k == 0) && (n == 0)) { u = qbeta(1:many/many, k_0, k_0 + 1);  d = qbeta(1:many/many, k_0 + 1, k_0) }
  else if (k == n)          { u = qbeta(1:many/many, k, k_0 + 1);    d = qbeta(1:many/many, k + 1, k_0) }
  else if (k == 0)          { u = qbeta(1:many/many, k_0, n - k_0 + 1);  d = qbeta(1:many/many, k_0 + 1, n - k_0)        }
  else                      { u = uu();         d = dd()        }
  list(left = u, right = d)
}

ebcbox <- function(j,N=1000) list(left=balch(j[[1]], j[[3]], N)$left, right=balch(j[[2]], j[[3]], N)$right)

breadth<-function(x){
  probs = 0:length(x[[1]])/length(x[[1]])
  s = 1/length(x[[1]])
  x_quants = quantile(x[[1]],probs=probs,type=1,names=FALSE)
  y_quants = quantile(x[[2]],probs=probs,type=1,names=FALSE)
  return(sum(s*(abs(x_quants-y_quants))))
}

ebc <- function(b,verbose=FALSE) {   
  x = b$left
  y = b$right
  N = length(x) - 1
  cbox=list(x,y)
  BR = breadth(cbox)
  ngood = max(which(breadths>BR))-1
  l_e = quantile(cbox[[1]], probs=.5,type=1,names=FALSE)
  r_e = quantile(cbox[[2]], probs=.5,type=1,names=FALSE)
  l_exp = sort(c(l_e,r_e))[1]
  r_exp = sort(c(l_e,r_e))[2]
  expectation = l_exp+((r_exp-l_exp)/2)
  kgood = round(ngood*expectation) #round(ngood*l_exp)
  k2good = kgood #round(ngood*r_exp)
  dsmall = 0
  #cat("l_exp:",l_exp," r_exp:",r_exp,"expectation",expectation,"\n")
  #cat("n:",ngood," k:",kgood,"\n")
  return(c(kgood,k2good,ngood,dsmall))
}

sayebc <- function(j,voc = c('patient','sick')) {
  if (j[1]==j[2]) cat('The chance the',voc[1],'is',voc[2],'is',j[1],'out of',j[3],'\n') else
    cat('The chance the',voc[1],'is',voc[2],'is',j[1],'to',j[2],'out of',j[3],'\n')
}

# k and n are expected to be single integers, but p is an array
cutbetaL = function(p, k, n) if (k==0) qbeta(p, k_0, n-k_0+1) else ifelse(p==0,qbeta(p_0, k, n-k+1),ifelse(p==1,qbeta(p_1, k, n-k+1),qbeta(p, k, n-k+1)))
cutbetaR = function(p, k, n) if (((k==0) && (n==0)) || (k==n)) qbeta(p, k_0+1, k_0) else ifelse(p==0,qbeta(p_0, k+1, n-k),ifelse(p==1,qbeta(p_1, k+1, n-k),qbeta(p, k+1, n-k)))

steps = 20

ijkL = c( 0, 1:(steps-1) / steps)
ijkR = c(    1:(steps-1) / steps, 1)

IJKL = expand.grid(ijkL,ijkL,ijkL)
IJKR = expand.grid(ijkR,ijkR,ijkR)

iL = IJKL[,3] 
jL = IJKL[,2] 
kL = IJKL[,1]
iR = IJKR[,3]
jR = IJKR[,2]
kR = IJKR[,1]


ppv = function(pk,pn, sk,sn, tk,tn) {
  p1 = cutbetaL(iL, pk, pn)
  p2 = cutbetaR(iR, pk, pn)
  s1 = cutbetaL(jL, sk, sn)
  s2 = cutbetaR(jR, sk, sn)
  t1 = cutbetaL(kL, tk, tn)
  t2 = cutbetaR(kR, tk, tn)
  aL = PPV(p1,s1,t1)
  aR = PPV(p2,s2,t2)  
  k <- 1:200
  L <- round(steps^3 / 200)
  list(left=sort(aL)[(k-1)*L + 1], right=sort(aR)[(k-1)*L + L])
}

npv = function(pk,pn, sk,sn, tk,tn) {
  p2 = cutbetaL(iL, pk, pn)  # p2, not p1, because npv is decreasing in p
  p1 = cutbetaR(iR, pk, pn)  # p1, not p2, because npv is decreasing in p
  s1 = cutbetaL(jL, sk, sn)
  s2 = cutbetaR(jR, sk, sn)
  t1 = cutbetaL(kL, tk, tn)
  t2 = cutbetaR(kR, tk, tn)
  aL = NPV(p1,s1,t1)
  aR = NPV(p2,s2,t2)  
  k <- 1:200
  L <- round(steps^3 / 200)
  list(left=sort(aL)[(k-1)*L + 1], right=sort(aR)[(k-1)*L + L])
}  

show = function(b,new=TRUE,xlab='Probability',...) {
  if (new) plot(NULL,xlim=c(0,1),ylim=c(0,1),ylab='Cumulative probability',xlab=xlab, ...)
  n <- length(b$left)
  s <- sort(b$left)
  r <- sort(b$right)
  lines(c(s,s[[n]],r[[n]]),c(0:(n-1)/n,1,1),type="S", ...)
  lines(c(s[1],r[1],r),c(0,0,1:(n)/n),type="s", ...)
}

showboth = function(pk,pn, sk,sn, tk,tn) {
  if (!checkkn(pk,pn, sk,sn, tk,tn)) stop('')
  ppv_answer = ppv(pk,pn, sk,sn, tk,tn)
  npv_answer = npv(pk,pn, sk,sn, tk,tn)
  show(ppv_answer,col='dark red',lwd=3,main="PPV")
  j = ebc(ppv_answer)
  show(ebcbox(j),new=FALSE,col='red',lwd=3)
  legend(.1,.9,legend=c("PPV","EBC"),fill=c('dark red','red'),cex = .55 )
  sayebc(j,c('patient','sick'))
  show(npv_answer,col='dark blue',lwd=3,main="NPV")
  jj = ebc(npv_answer)
  show(ebcbox(ebc(npv_answer)),new=FALSE,col='blue',lwd=3)
  legend(.1,.9,legend=c("NPV","EBC"),fill=c('dark blue','blue'),cex = .55 )
  sayebc(jj,c('patient','well'))
}

#########################################
# example data to exercise the functions
#########################################
# par(mfrow=c(2,4))
# showboth(2, 8,  30, 90,  80,81)
# showboth(60, 100,  2, 3,  80,81)
# showboth(60, 100,  10, 30,  40,81)
# showboth(60, 100, 30,90, 80,81)