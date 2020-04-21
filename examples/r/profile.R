profile <- function(lines = FALSE) {
    Rprof(tmp <- tempfile(), line.profiling=lines)
    eval(parse(file = "mmm.R", keep.source=TRUE))
    Rprof()
    if (lines) {
       lines_show <- "show"
    } else {
       lines_show <- "hide"
    }
    summaryRprof(tmp, lines = lines_show)
}

l <- c(FALSE, TRUE)
sapply(l, profile)

