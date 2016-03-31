library(data.table)
add_new <- function(x, new_vrb, all_poss_val) {
  tmp <- matrix(NA, nrow = length(all_poss_val)^length(new_vrb), ncol = length(new_vrb)) # pre-allocate
  tmp <- expand.grid(rep(list(all_poss_val), length(new_vrb)))
  tmp <- as.data.table(tmp)
  names(tmp) <- new_vrb
  x_out <- x[, cbind(tmp), by = names(x)]
  criteria <- combn(names(x_out), 2)
  criteria <- paste("[", criteria[1, ], " != ", criteria[2, ], "]", sep = "", collapse = "")
  eval(parse(text = paste("x_out <- x_out", criteria, sep = "")))
  x_out <- setcolorder(x_out, order(names(x_out)))
  return(x_out)
}

convert_wr_base <- function(x, base) {
  # Convert the number to desired written presentation based on the base
  if (base > (10 + 26)) stop("Maximum Base supported is 36 (0-Z)")
  digit <- c(0:9, LETTERS)[1:base]
  digit[x + 1]
}


#================================#
# Solve AB - CD = EF & EF + GH = PPP
#================================#
solve_w2 <- function(base = 10, allow_zero = FALSE) {
  # allow_zero = FALSE, the most significant digit is not allowed to be zero
  ## All possible values of E and G
  choice_sig <- (!allow_zero):(base - 1) # choice of the most significant digit
  choice_nsig <- 0:(base - 1) # chose of non most significant digits
  
  sol <- matrix(NA, nrow = length(choice_sig)^2, ncol = 2) # pre-allocate
  sol <- expand.grid(rep(list(choice_sig), 2))
  names(sol) <- c("e", "g")
  sol <- as.data.table(sol)[e != g][order(e)]
  
  ## Find all possible values of  P
  sol <- rbind(sol[, p := trunc((e + g)/ base)], sol[, p := trunc((e + g + 1)/ base)])
  sol <- sol[p != e][p != g]
  sol <- sol[p %in% choice_sig]
  ## Use P to exclude (E, G, P) which is not possible to be the solution
  sol <- sol[(e + g) <= (p * (base + 1))][(e + g) >= (p * (base + 1) - 1)]
  ## Find all possible values of F
  sol <- add_new(sol, "f", choice_nsig)
  ## Find H given (E, F, G, P)
  sol <- sol[, h := p * (base^2 + base + 1) - (e * base + f + g * base)][h >= 0][h < base]
  sol <- sol[h != e][h != f][h != g][h != p]
  ## Find all possible values of B
  sol <- add_new(sol, "b", choice_nsig)
  ## Find D given (B, E, F, G, H, P)
  sol <- sol[, d := b - f + base * (b < f)][d >= 0][d < base]
  sol <- sol[d != b][d != e][d != f][d != g][d != h][d != p]
  ## Find all possible values of A
  sol <- add_new(sol, "a", choice_sig)
  ## Find A given (A, B, D, E, F, G, H, P)
  sol <- sol[, c := a - e - (b < d)][c %in% choice_sig][c < base]
  sol <- sol[c != a][c != b][c != d][c != e][c != f][c != g][c != h][c != p]
  
  sol <- sol[order(a, b, c, d, e, f, g, h, p), list(a, b, c, d, e, f, g, h, p)]
  sol[] <- lapply(sol, convert_wr_base, base = base)
  return(sol)
}




# Output
sink(file = "D:/Users/perry/Dropbox/mypersonal/ABCDEFGHPPP/output_w2.txt")
cat("Machine: i7-4790; RAM: 12GB; 64-bit\n\n")
cat("----------------------------------------------------------------\n\n")
for (base in c(16, 22, 28, 34)) {
  time <- system.time({sol <- solve_w2(base)})
  
  cat("# Base", base, "- Width 2", "\n\n")
  cat("Total number of solutions:", nrow(sol), "\n\n")
  cat("First 50 solutions: \n")
  for (i in 1:50) {
    with(sol[i], cat(format(i, width = nchar(50)), ": ", a, b, " - ", c, d, " = ", e, f, "  &  ", e, f, " + ", g, h, " = ", p, p, p, "\n", sep = ""))
  }
  cat("\n")
  cat("Time Used:", time[1], "(user)", time[2], "(system)", time[3], "(elapsed) \n\n")
  cat("----------------------------------------------------------------\n\n")
}
sink()




