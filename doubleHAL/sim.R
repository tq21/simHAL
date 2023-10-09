source("utils.R")

n_vals <- c(500, 1000, 2000)

for (n in n_vals) {
  res <- run_sim(n = n,
                 seed = 32897,
                 type = "all")

  saveRDS(res, file = "out/1005_" %+% n %+% ".RDS")
}
