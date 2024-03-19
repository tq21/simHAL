library(hal9001)
source("utils.R")
devtools::load_all()
`%+%` <- function(x, y) paste0(x, y)

deltas <- seq(0.5, 10, 0.1)

res_jump <- run_sim(n = 500,
                    d = 5,
                    scenario = "jump",
                    n_noise = 0,
                    deltas = deltas)
saveRDS(res_jump, "out/1120_jump.RDS")

res_smooth <- run_sim(n = 500,
                      d = 5,
                      scenario = "smooth",
                      n_noise = 0,
                      deltas = deltas)
saveRDS(res_smooth, "out/1120_smooth.RDS")

res_sin <- run_sim(n = 500,
                   d = 5,
                   scenario = "sin",
                   n_noise = 0,
                   deltas = deltas)
saveRDS(res_sin, "out/1120_sin.RDS")
