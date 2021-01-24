library("deSolve")

`behroozi` <- function(t, state, parameters) {  # t == theta
    with(as.list(c(state, parameters)), {
        denominator <-   z + 2/R - sin(t)/r
        dr <- cos(t)/denominator
        dz <- sin(t)/denominator
        list(c(dr, dz))
    })
}

`integrate` <- function(R=1, theta=seq(0, pi*0.9, by = 0.01)){
    state      <- c(r = 0.0001, z = 0)
    parameters <- c(R=R)
    out <- as.matrix(ode(y=state,times=theta,func=behroozi,parms=parameters))
    colnames(out)[1] <- "theta"
    out
}

`area` <- function(x,y){
    if(is.matrix(x)){
        x <- x[,1]
        y <- x[,2]
    }
    stopifnot(length(x)==length(y))
    n <- length(x)
    abs(sum((x[-n]*y[-1] - x[-1]*y[-n])/2))
}

`volume_curve` <- function(x,y){
    if(is.matrix(x)){
        y <- x[,2]  # not the other way round, otherwise x gets overwritten!
        x <- x[,1]

    }
    stopifnot(length(x)==length(y))
    (2*pi/3)*sum(abs(x[-1]^2*diff(y)))  # there must be a better way
}

`get_volume` <- function(out){ # out == integrate(R,theta)
    n <- nrow(out)
    r <- out[n,2]
    z <- out[n,3]
    volume_curve(out[,2:3]) + pi*r^2*z/3
}

`get_things` <- function(R=1,theta=seq(0, pi*0.9, by = 0.01)){
    out <- integrate(R,theta)
    colnames(out) <- NULL
    n <- nrow(out)
    jj <- out[c(n-1,n),2:3]
    return(c(z=out[nrow(out),3],
             contact_angle = atan2(
                 jj[1,1]-jj[2,1],
                 jj[2,2]-jj[1,2]),
             volume=get_volume(out)
           ))
}

`plot_drop` <- function(R=1, theta=seq(0, pi*0.9, by = 0.01),...){
    out <- integrate(R=R,theta=theta)
    out[,3] <- -out[,3]
    jj <- out
    jj[,2] <- -jj[,2]
    
    z_end <- jj[nrow(jj),3]
    out <- rbind(out[nrow(out):1,],jj)
    plot(out[,2:3],asp=1,type='l',...)
    abline(h=z_end)
    segments(x0=0,y0=0, x1=out[nrow(out),2],y1=out[nrow(out),3])
}

plot_drop()

