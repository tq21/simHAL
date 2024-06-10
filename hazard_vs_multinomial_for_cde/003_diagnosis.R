


fit_obj <- res_500[[1]]$hal_haz_reg_fit
fit_obj$basis_list[which(fit_obj$coefs[-1] != 0)]

fit_obj_2 <- res_500[[1]]$hal_multinom_fit
fit_obj_2$basis_list[which(as.numeric(fit_obj_2$coefs[[1]]) != 0)]
fit_obj_2$basis_list[which(as.numeric(fit_obj_2$coefs[[2]]) != 0)]
fit_obj_2$basis_list[which(as.numeric(fit_obj_2$coefs[[3]]) != 0)]
