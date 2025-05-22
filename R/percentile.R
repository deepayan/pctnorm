



percentile <- function(x, method = c("brute", "rank"))
{
    if (!is.numeric(x)) stop("Input scores must be numeric")
    if (anyNA(x)) stop("Missing scores are not allowed")
    N <- length(x)

    ## For each score, want number of scores less than or equal to
    ## it. We can be smart about this (counting sort) but let's do
    ## this by brute force as a reference implementation

    countLess <- function(s) sum(x <= s)

    D <- switch(method,
                brute = sapply(x, countLess),
                rank = rank(x, ties.method = "max"))

    list(D = D, N = N, P = D / N)
}

if (FALSE)
{


## 50000 is already slow

n <- 25000
x <- round(150 * runif(n))
system.time(A <- percentile(x, method = "brute"))
system.time(B <- percentile(x, method = "rank"))

identical(A, B)

## percentile(x, method = "brute")$D
## percentile(x, method = "rank")$D


}
