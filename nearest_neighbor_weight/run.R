library(hal9001)
source("utils.R")
devtools::load_all()

res_1 <- run_sim(n = 500,
                 d = 5,
                 scenario = "jump",
                 n_noise = 0,
                 delta = 0.5)

res_2 <- run_sim(n = 500,
                 d = 5,
                 scenario = "jump",
                 n_noise = 0,
                 delta = 1)

res_3 <- run_sim(n = 500,
                 d = 5,
                 scenario = "jump",
                 n_noise = 0,
                 delta = 1.5)

res_4 <- run_sim(n = 500,
                 d = 5,
                 scenario = "jump",
                 n_noise = 0,
                 delta = 5)
