library(pareto)
stopifnot(is.nan(dpareto(3,-2, 1))) # bad parameter
stopifnot(is.nan(dpareto(3,2, -1))) # bad parameter
stopifnot(all.equal(dpareto(3,2,1), 0.2222222222))
stopifnot(all.equal(dpareto(1,2,3), 0.0))
stopifnot(all.equal(dpareto(3:5,2, 1), c(0.2222222222, 0.1250000, 0.0800000))) 
stopifnot(all.equal(dpareto(1:5,2, 1), c(0.0, 0.0, 0.2222222222, 0.1250000, 0.0800000))) 
stopifnot(all.equal(dpareto(6,2:4, 1), c(0.05555555556, 0.08333333333, 0.11111111111))) 
stopifnot(all.equal(log(dpareto(1:5,2, 1)), dpareto(1:5,2, 1, log = TRUE)))
stopifnot(all.equal(dpareto(1:6,1:2, 1),c(0.0, 0.0, 0.11111111111, 0.125, 0.04, 0.05555555556))) 
stopifnot(all.equal(dpareto(1, 2, 1:2), c(0, 0)))