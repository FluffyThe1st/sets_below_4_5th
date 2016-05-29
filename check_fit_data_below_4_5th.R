# models <- c("std", "skew", "mix", "fleish", "ML")
library(rstan)
check_fit_below_4_5th <- function(cond, model)
{
  if (cond == 1 | cond == 4 | cond == 7 | cond == 10) {NB <- 15}
  if (cond == 2 | cond == 5 | cond == 8 | cond == 11) {NB <- 25}
  if (cond == 3 | cond == 6 | cond == 9 | cond == 12) {NB <- 50}
  below <- NULL
  for (j in 1:100)
  {
    n_T <- NULL; n_B <- NULL
    load(paste0("./FIT_DATA/", model, "_cond", cond, "_set", j, ".Rdata"))
    test <- monitor(extract(mdl_fit, c("theta", "beta"), permuted = F, inc_warmup = F), probs = F)
    n_T <- sum(test[1:300, "Rhat"] < 1.1)
    n_B <- sum(test[301:(300 + NB)] < 1,1)
    if (n_T < (300 / 5 * 4) | n_B < (NB / 5 * 4)) {below <- c(below, j)}
  }
  assign(paste0("sets_of_cond", cond, "_below_4_5th"), below, envir = .GlobalEnv)
  remove(test, mdl_fit, n_T, NB, n_B, below, j, model)
  save.image(file = paste0("sets_of_cond", cond, "below_4_5th.Rdata"))
}