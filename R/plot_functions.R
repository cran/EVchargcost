plot_functions <- function(a, alpha, beta, delta, gamma, tau, R, x_values, y_values){
    #' @title Plots the charging function, the electricity price function and the optimal cost function
    #' @description Function that plots the charging function, the electricity price function and the optimal cost function
    #' @param a vector with the breaking points of charging function in the x-axis
    #' @param alpha vector with the slopes of the charging function on each segment
    #' @param beta vector with the y-intercepts of the charging function on each segment
    #' @param delta vector with the times duration of each segment of electricity price function
    #' @param gamma vector with the prices of the electricity on each segment of electricity price function
    #' @param tau consumption of the vehicle (numerical value)
    #' @param R range of the vehicle (numerical value)
    #' @param x_values vector with the x-values of the breaking points of the charging cost function
    #' @param y_values vector with the y-values of the breaking points of the charging cost function
    #' @return A plot with the charging function, the electricity price function and the optimal cost function
    #' @examples
    #' a <- c(0,3.3,6.6,10)
    #' alpha <- c(0.1757576, 0.07272727, 0.05294118)
    #' beta <- c(0, 0.34, 0.4705882)
    #' delta <- c(4, 3, 5)
    #' gamma <- c(0.45, 0.25, 0.5)
    #' tau <- 0.15
    #' R <- 250
    #' opt_cost_function = minimum_cost(a, alpha, beta, delta, gamma, tau, R)
    #' xvalues <- opt_cost_function[["xvalues"]]
    #' yvalues <- opt_cost_function[["yvalues"]]
    #' plot_functions(a, alpha, beta, delta, gamma, tau, R, xvalues, yvalues)
    B <- length(alpha)
    P <- length(gamma)

    x <- a
    y <- c(0)
    for (i in 1:B){
        y <- c(y, alpha[i]*a[i+1]+beta[i])
    }
    df1 <- data.frame(x = x, y = y)
    plot1 <- ggplot2::ggplot(df1, ggplot2::aes(x, y)) + ggplot2::geom_line(color = "darkblue") + labs(x = "Duration", y = "State of Charge", title = "Charging function") + ggplot2::geom_point(size=1, color="darkblue")

    xinit <- c(0)
    xend <- c()
    for (i in 1:(P-1)){
        xinit <- c(xinit, sum(delta[1:i]))
        xend <- c(xend, sum(delta[1:i]))
    }
    xend <- c(xend, sum(delta))
    y <- gamma
    df2 <- data.frame(xinit = xinit, y = y, xend = xend)
    plot2 <- ggplot2::ggplot(df2, ggplot2::aes(x, y)) + ggplot2::geom_segment(ggplot2::aes(x = xinit, y = y, xend = xend, yend = y), linetype = "solid", color = "darkorange") + labs(x = "Time", y = "Price", title = "Electricity price function")

    x <- x_values
    y <- y_values
    df3 <- data.frame(x = x, y = y)
    plot3 <- ggplot2::ggplot(df3, ggplot2::aes(x, y)) + ggplot2::geom_line(color = "darkgreen") + labs(x = "State of Charge", y = "Cost", title = "Charging cost function")  + ggplot2::geom_point(size=1, color="darkgreen")

    p <- cowplot::plot_grid(plot1, plot2, plot3, ncol = 3)
    return(p)
}
