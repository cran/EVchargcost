minimum_cost <- function(a, alpha, beta, delta, gamma, tau, R) {
    #' @title Computes the minimum cost function
    #' @description Function that computes the minimal cost function for a given charging function (given by a, alpha and beta), an electricity price function (given by delta and gamma), a consumption tau and a range R
    #' @param a vector with the breaking points of charging function in the x-axis
    #' @param alpha vector with the slopes of the charging function on each segment
    #' @param beta vector with the y-intercepts of the charging function on each segment
    #' @param delta vector with the times duration of each segment of electricity price function
    #' @param gamma vector with the prices of the electricity on each segment of electricity price function
    #' @param tau consumption of the vehicle (numerical value)
    #' @param R range of the vehicle (numerical value)
    #' @return list with the x-values and y-values of the minimum cost function
    #' @examples
    #' a <- c(0,3.3,6.6,10)
    #' alpha <- c(0.1757576, 0.07272727, 0.05294118)
    #' beta <- c(0, 0.34, 0.4705882)
    #' delta <- c(4, 3, 5)
    #' gamma <- c(0.45, 0.25, 0.5)
    #' tau <- 0.15
    #' R <- 250
    #' opt_cost_function = minimum_cost(a, alpha, beta, delta, gamma, tau, R)
    #' print(opt_cost_function)
    B <- length(alpha)
    P <- length(gamma)
    SoC <- function(x){
        if (x >= a[B+1]){
            return(1)
        }
        for (i in 1:(B+1)){
            if (x < a[i]){
                return (alpha[i-1]*x+beta[i-1])
            }
        }
    }
    w <- rep(0, P)
    barT <- rep(0, P)
    xvalues <- c(0)
    yvalues <- c(0)
    C <- 0
    while (SoC(barT[P]) < 1){
        phi <- rep(1e10, P)
        eps <- a[B+1]-barT[P]
        for (p in 1:P){
            if (w[p] < delta[p]){
                min_ab <- 1e10
                for (b in 1:(B+1)){
                    if (a[b] > barT[p] && a[b] < min_ab){
                        min_ab <- a[b]
                    }
                }
                if (min_ab-barT[p] < eps){
                    eps <- min_ab-barT[p]
                }
            }
        }
        for (p in 1:P){
            if (w[p] < delta[p]){
                phi[p] <- tau*R*gamma[p]*(SoC(barT[p]+eps)-SoC(barT[p]))
                for (p2 in 1:P){
                    if (p2 > p){
                        phi[p] <- phi[p] + tau*R*gamma[p2]*((SoC(barT[p2]+eps)-SoC(barT[p2]))-(SoC(barT[p2-1]+eps)-SoC(barT[p2-1])))
                    }
                }
            }
        }
        pstar <- which.min(phi)
        eps <- min(delta[pstar]-w[pstar], eps)

        C <- C + tau*R*gamma[pstar]*(SoC(barT[pstar]+eps)-SoC(barT[pstar]))
        w[pstar] <- w[pstar] + eps
        barT[pstar] <- barT[pstar] + eps
        for (p in 1:P){
            if (p > pstar){
                barT[p] <- barT[p] + eps
                C <- C + tau*R*gamma[p]*((SoC(barT[p])-SoC(barT[p]-eps))-(SoC(barT[p-1])-SoC(barT[p-1]-eps)))
            }
        }
        xvalues <- c(xvalues, SoC(barT[P]))
        yvalues <- c(yvalues, C)
    }
    return(list(xvalues = xvalues, yvalues = yvalues))
}


