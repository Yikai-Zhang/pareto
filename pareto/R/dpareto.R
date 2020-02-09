dpareto <- function(x, a, b, log = FALSE) { 
    nx <- length(x) 
    na <- length(a) 
    nb <- length(b) 
    n <- max(nx, na, nb) 
    if (nx < n) x <- rep(x, length.out = n) 
    if (na < n) a <- rep(a, length.out = n) 
    if (nb < n) b <- rep(b, length.out = n) 
    ld <- ifelse(a > 0 & b > 0, 
                 ifelse(x > a, 
                        log(b) + b * log(a) - (b + 1) * log(x), 
                        log(0)), 
                 NaN) 
    if (log) ld 
    else exp(ld)

}
