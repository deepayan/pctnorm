

## Input: vector of scores for multiple sessions. For now, assume that
## inputs are named vectors, with names giving student identifiers.



normalize <- function(..., nsmall = 10)
{
    slist <- list(...)
    plist <- lapply(slist, percentile, nsmall = nsmall)

    ## next step is to merge by percentile, but this is a bit
    ## tricky. Dimension of result is: nrow = number of unique
    ## percentiles, ncol = length(slist)

    ## NOTE: For the normalized score calculations, we only need the
    ## unique percentiles / scores

    ulist <- lapply(plist, function(d) unique(d[c("pchar", "raw")]))
    
    ## step 1: find unique percentiles

    upchar <- lapply(plist, getElement, "pchar") |> unlist() |> unique()

    ## step 2: construct matrix with NA entries

    allScores <- matrix(NA_real_, nrow = length(upchar), ncol = length(ulist))
    rownames(allScores) <- rev(sort(upchar))

    ## step 3: fill-in observed values

    for (i in seq_along(ulist)) {
        allScores[ulist[[i]]$pchar, i] <- ulist[[i]]$raw
    }

    ## step 4: interpolate missing scores (column-wise)

    pp <- as.numeric(rownames(allScores))

    for (i in seq_len(ncol(allScores))) {
        ok <- is.finite(allScores[, i])
        ## print(table(ok))
        allScores[!ok, i] <-
            approx(x = pp[ok], y = allScores[ok, i],
                   xout = pp[!ok], method = "linear")$y
    }

    ## step 5: take row-average of scores to get normalized score as
    ## function of percentile

    norm_scores <- rowMeans(allScores)

    ## Finally, add normalized score to data for each shift, and then
    ## return combined merit list sorted by percentile

    for (i in seq_along(plist)) {
        plist[[i]]$normScore <- norm_scores[plist[[i]]$pchar]
    }
    
    do.call(rbind, plist) |> sort_by(~ I(-percentile) + id)
    
}



## testing

if (FALSE)
{

simScore <- function(n, mean = 50, sd = 10)
{
    rnorm(n, mean = mean, sd = sd) |> round() |> pmin(100) |> pmax(0)
}

x1 <- simScore(100000, 60, 10)
x2 <- simScore(100200, 50, 20)

names(x1) <- paste0("A", seq_along(x1))
names(x2) <- paste0("B", seq_along(x2))
    
d <- normalize(x1, x2)

}
