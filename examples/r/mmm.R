initialize_matrices <- function(i, j, k) {
    matrices <- list(
        "a" = matrix(rexp(i*j), nrow=i, ncol=j),
        "b" = matrix(rexp(j*k), nrow=j, ncol=k),
        "c" = matrix(0L, nrow=i, ncol=k),
        "i" = i, "j" = j, "k"= k)
    return(matrices)
}

r_gemm <- function(matrices) {
    start <- Sys.time()
    matrix_c <- matrices$a %*% matrices$b
    stop <- Sys.time()
    return(stop-start)
}

n_gemm <- function(matrices) {
    start <- Sys.time()
    for (i in 1:matrices$i) {
        for (j in 1:matrices$j) {
            for (k in 1:matrices$k) {
                matrices$c[i, j] = matrices$c[i, j] + 
                                   matrices$a[i, k] * 
                                   matrices$b[k, j]
            }
        }
    }
    stop <- Sys.time()
    return(stop-start)
}

compare_gemm <- function(i) {
    matrices <- initialize_matrices(i, i, i)
    results <- list(
        "r_gemm_dt" = r_gemm(matrices),
        "n_gemm_dt" = n_gemm(matrices))
    return(results)
}

compare <- function() {
    i <- c(100, 150, 200)
    sapply(i, compare_gemm)
}

profile_n_gemm <- function() {
    matrices <- initialize_matrices(200, 200, 200)
    n_gemm(matrices)
}

main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    if (length(args) == 0) {
        profile_n_gemm()
    } else {
        compare()
    }
}

main()

