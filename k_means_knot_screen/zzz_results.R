library(dplyr)
library(xtable)

res_benchmark <- readRDS("out/003_benchmark.rds")
tab_benchmark <- res_benchmark %>%
  summarize(mean_r_squared_quant = mean(r_squared_quant),
            mean_r_squared_kmeans = mean(r_squared_kmeans),
            mean_tol_bases_quant = mean(tol_bases_quant),
            mean_tol_bases_kmeans = mean(tol_bases_kmeans),
            .by = c("d", "scenario"))

res_skewed_benchmark <- readRDS("out/004_skewed_benchmark.rds")
tab_skewed_benchmark <- res_skewed_benchmark %>%
  summarize(mean_r_squared_quant = mean(r_squared_quant),
            mean_r_squared_kmeans = mean(r_squared_kmeans),
            mean_tol_bases_quant = mean(tol_bases_quant),
            mean_tol_bases_kmeans = mean(tol_bases_kmeans),
            .by = c("d", "scenario"))

xtable(tab_benchmark, digits = 3)
xtable(tab_skewed_benchmark, digits = 3)


