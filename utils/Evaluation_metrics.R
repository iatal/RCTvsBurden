#True positives for gbd category in numeric(1:27)
TP <- function(algo,data,gbd){
sum(as.numeric(sapply(data,function(x){gbd%in%as.numeric(x)}))*
    as.numeric(sapply(algo,function(x){gbd%in%as.numeric(x)})))
}

#False positives
FP <- function(algo,data,gbd){
sum(as.numeric(sapply(data,function(x){!gbd%in%as.numeric(x)}))*
    as.numeric(sapply(algo,function(x){gbd%in%as.numeric(x)})))
}

#True negatives
TN <- function(algo,data,gbd){
sum(as.numeric(sapply(data,function(x){!gbd%in%as.numeric(x)}))*
    as.numeric(sapply(algo,function(x){!gbd%in%as.numeric(x)})))
}

#False negatives
FN <- function(algo,data,gbd){
sum(as.numeric(sapply(data,function(x){gbd%in%as.numeric(x)}))*
    as.numeric(sapply(algo,function(x){!gbd%in%as.numeric(x)})))
}

#Confusion matrix for a list of categories
conf_matrix <- function(algo,data,lab_list){
cm <- do.call('rbind',lapply(lab_list,function(l){c(TP(algo,data,l),
                                              FP(algo,data,l),
                                              TN(algo,data,l),
                                              FN(algo,data,l))}))
colnames(cm) <- c("TP","FP","TN","FN")
    rownames(cm) <- lab_list
    return(cm)
}

#Functions for deriving sensitivities and specificities based on confusion matrix
#And 95% confidence intervals of a proportion

interv_prop <- function(r,n,alpha){

    if(r==0 & n==0) return(c(0,1))
    if(r!=0 & r!=n){
        A <- 2*r + qnorm(1-(alpha/2))^2
        B <- sqrt(qnorm(1-(alpha/2))^2 + 4*r*(1-r/n))
        C <- 2*((n)+qnorm(1-(alpha/2))^2)
        lower.int <- (A-B)/C
        upper.int <- (A+B)/C
    }

    if(r==0){
        lower.int <- 0
        upper.int <- (qnorm(1-(alpha/2))^2)/(n+qnorm(1-(alpha/2))^2)
    }

    if(r==n){
        lower.int <- n/(n+qnorm(1-(alpha/2))^2)
        upper.int <- 1
    }

    #this method may lead to confidence intervals not including the proportion
    #in particular for rare events
    #or to confidence intervals lower than 0 or higher than 1
    #for these cases we estimate the confidence interval using R function binom.test
    
    if(lower.int<0 | upper.int>1 | r/n>upper.int | r/n<lower.int){
        lower.int <- binom.test(r,n,0.5)$conf.int[1]
        upper.int <- binom.test(r,n,0.5)$conf.int[2]
        }

    return(c(lower.int,upper.int))
}

metr.ci <- function( m, alpha=0.05 ) {

  a <- m[1, 1]
  b <- m[1, 2]
  c <- m[2, 1]
  d <- m[2, 2]

  sens <- a/(a+c)
  CI.sens <- interv_prop(a,a+c,alpha)

  spec <- d/(b+d)
  CI.spec <- interv_prop(d,b+d,alpha)

 lr.pos <- sens/(1 - spec)  

    sigma2 <- (1/a) - (1/(a+c)) + (1/b) - (1/(b+d))
    lower.pos <- lr.pos * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    upper.pos <- lr.pos * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 

 lr.neg <- (1 - sens)/spec

    sigma2 <- (1/c) - (1/(a+c)) + (1/d) - (1/(b+d))
    lower.neg <- lr.neg * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    upper.neg <- lr.neg * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 

 list(
    sens=sens, lower.sens=CI.sens[1], upper.sens=CI.sens[2],
    spec=spec, lower.spec=CI.spec[1], upper.spec=CI.spec[2],
    lr.pos=lr.pos, lower.pos=lower.pos, upper.pos=upper.pos,
    lr.neg=lr.neg, lower.neg=lower.neg, upper.neg=upper.neg
    )

}
