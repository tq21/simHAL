library(ggplot2)
library(ggpubr)

plt_1 <- readRDS("figs/plt_scenario_1.rds")
plt_2 <- readRDS("figs/plt_scenario_2.rds")
plt <- ggarrange(plt_1, plt_2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
ggsave(filename = "hazard_vs_multinomial.pdf", plot = plt, device = "pdf",
       path = "figs", width = 8, height = 4, dpi = 300)
