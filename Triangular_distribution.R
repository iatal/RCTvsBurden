#Deriving a triangular distribution for a given mean and confidence interval

#mn represents estimated mean
#low and up estimated confidence interval
#q1 upper and lower range of confidence interval
#N: number of steps to find a solution

#output are Af, Bf and Cf: limits and mode of triangular distribution

triang_distrib_limits <- function(mn=mn,low=low,up=up,q1=0.025,N=10000){
        
    M <- mn
    Q1 <- low
    Q3 <- up

    #If mn = low or = up, the output is a truncated triangular distribution
    #with a probability mass of 2.5% at the truncated size
    if(M==Q1){
        Af <- M
        Cf <- M
        K <- Q3 - Q1
        #solution found by solving polyn 2d degree
        a <- (2*q1 - 1)/(1-q1)
        b <- 2*q1*K/(1-q1)
        c <- q1*(K^2)/(1-q1)
        delta <- b^2 - 4*a*c
        if(delta < 0){print('no solution')
                      return(NA)}
        x <- (-b - sqrt(delta))/(2*a)
        Bf <- Q3 + x
        return(c(Af,Cf,Bf))
    }
    
    if(M==Q3){
        Bf <- M
        Cf <- M
        K <- Q3 - Q1
        #solution found by solving polyn 2d degree
        a <- (2*q1 - 1)/(1-q1)
        b <- 2*q1*K/(1-q1)
        c <- q1*(K^2)/(1-q1)
        delta <- b^2 - 4*a*c
        if(delta < 0){print('no solution')
                      return(NA)}
        x <- (-b - sqrt(delta))/(2*a)
        Af <- Q1 - x
        return(c(Af,Cf,Bf))
    }
    
    theta <- atan(-2/(1+q1))/2
    K <- Q3-Q1
    #Ax^2 + Cy^2 + 2Dx + 2Ey + F = 0
    A <- 1+ cos(theta)^2 -q1*sin(theta)^2 - sin(2*theta)
    C <- 1+ sin(theta)^2 -q1*cos(theta)^2 + sin(2*theta)
    D <- K*(cos(theta)-sin(theta))
    E <- -K*(cos(theta)+sin(theta))
    F1 <- K^2 - (D^2)/A - (E^2)/C
    if(sign(A*C)<0 | sign(A*F1)>0 | sign(C*F1)>0){
        print('No solution')
        return(NA)
    }

    #If a solution exists we have an ellipse
    #X^2/a^2 + Y^2/b^2 = 1
    a <- sqrt(-F1/A)
    b <- sqrt(-F1/C)
    
    #We need to intersect the ellipse with a line
    #z = variable [AC]
    #Y = u*X + v + w*z
    u <- (3*cos(theta) - sin(theta))/(cos(theta)+3*sin(theta))
    v <- E/C - (D/A)*u + 3*(M-Q1)/(cos(theta)+3*sin(theta))
    w <- -1/(cos(theta)+3*sin(theta))

    #When insering line equation in ellipse, X is a solution of polyn 2d degree
    #With delta depending on z

    #delta is positif if and only if (v+w*z)^2 <= b^2 + (u*a)^2
    V <- sqrt(b^2 + (u*a)^2)

    #z must be between (-V - v)/w and (V-v)/w
    zm <- (-V - v)/w
    zM <- (V-v)/w

    delta <- function(z){
        if(z==zM | z==zm) return(0)
        else 4*((1/a)^2 + (u/b)^2 - ((v+w*z)/(a*b))^2)
        }

    #For z fixed we have X and Y
    X1 <- function(z){
        (-(2*u*(v+w*z))/(b^2) + sqrt(delta(z)))/(2*((1/a)^2 + (u/b)^2))
        }
    X2 <- function(z){
        (-(2*u*(v+w*z))/(b^2) - sqrt(delta(z)))/(2*((1/a)^2 + (u/b)^2))
        }
    Y <- function(X,z){
        u*X+v+w*z
        }

    #For (X,Y) solution, (x,y) = (AQ1,AB) are given by
    x <- function(X,Y){
        X*cos(theta)-Y*sin(theta)+(-D*cos(theta)/A + E*sin(theta)/C)
        }
    y <- function(X,Y){
        X*sin(theta)+Y*cos(theta)+(-D*sin(theta)/A - E*cos(theta)/C)
        }

    #We visit [zm;zM] with z by N steps
    t <- zM - zm
    dt <- t/N

    DF <- data.frame()
    
    for(i in 1:N){

        zf <- zm + dt*(i-1)
        xf <- x(X1(zf),Y(X1(zf),zf))
        yf <- y(X1(zf),Y(X1(zf),zf))

        if(xf>0 & yf>0 & zf>0 & xf<zf & zf<yf){
            DF <- rbind(DF,c(i,1,
                             2*xf^2 + (1-q1)*yf^2 - 2*xf*yf + 2*K*(xf-yf) + K^2,
                             3*M-3*Q1+3*xf-yf-zf,
                             q1-xf^2/(yf*zf),
                             q1-((yf-K-xf)^2)/((yf-zf)*yf)
                            ))
        }

        xf <- x(X2(zf),Y(X2(zf),zf))
        yf <- y(X2(zf),Y(X2(zf),zf))

        if(xf>0 & yf>0 & zf>0 & xf<zf & zf<yf){
            DF <- rbind(DF,c(i,2,
                             2*xf^2 + (1-q1)*yf^2 - 2*xf*yf + 2*K*(xf-yf) + K^2,
                             3*M-3*Q1+3*xf-yf-zf,
                             q1-xf^2/(yf*zf),
                             q1-((yf-K-xf)^2)/((yf-zf)*yf)
                            ))
        }
    }

    if(nrow(DF)==0){
        print('No solution')
        return(NA)
        }
    names(DF) <- c("step","sqr","ellipse","mean","Q1","Q3")
    
    #When possible, Q1 percentile lower than actual Q1
    if(sum(DF$Q1>=0)>0) DF <- DF[DF$Q1>=0,]
        
    j <- DF$step[abs(DF$Q1)+abs(DF$Q3)==min(abs(DF$Q1)+abs(DF$Q3))]
    s_q <- DF$sqr[abs(DF$Q1)+abs(DF$Q3)==min(abs(DF$Q1)+abs(DF$Q3))]

    if(length(j)>1){
        print('several steps') 
        return(NA)        
    }

    zf <- zm + dt*(j-1)

    if(s_q==2){
        xf <- x(X2(zf),Y(X2(zf),zf))
        yf <- y(X2(zf),Y(X2(zf),zf))
        }

    if(s_q==1){
        xf <- x(X1(zf),Y(X1(zf),zf))
        yf <- y(X1(zf),Y(X1(zf),zf))
        }
            
    Af <- Q1-xf
    Cf <- Af+zf
    Bf <- Af+yf

    return(c(Af,Cf,Bf))
            
}

#Triangular distrib: depending if it is truncated or not
            
            