# pure R version
source("./functions.R", local = TRUE)

library(openxlsx)

# pull from excel file of user's choosing:

xl_loc <- rstudioapi::selectFile()
xl_loc <- normalizePath(xl_loc)

file.exists(xl_loc)


# Take named range for TPMs out directly

wb <- loadWorkbook(xl_loc)

TPM <- read.xlsx(wb, namedRegion = "TPM_VP_Y1_orig", colNames = FALSE)

start_states <- rep(0.2,5)

TPM %*% start_states

# going from annual transitions to 28 day (4-week) cycles, 28*13 = 364 so it's very close
# but to get it precise:
nth_root <- 365.25/28

solved <- TPM_time_machine(MAT = TPM[1:(nrow(TPM)-1),2:ncol(TPM)],nth_root = nth_root)


tester <- Reduce(
  accumulate = TRUE,
  x = 1:10,
  init = c(1,0,0,0, 0),
  f = function(prev, cycle) {
    prev  %*% TPM
  }
)



plot(do.call(rbind,tester)[,1], type = "l")
plot(do.call(rbind,tester)[,2], type = "l")
plot(do.call(rbind,tester)[,3], type = "l")
plot(do.call(rbind,tester)[,4], type = "l")


TPM2 <- TPM[1:(nrow(TPM)-1),2:ncol(TPM)]

TPM_time_machine(MAT = TPM2,nth_root = nth_root)


TPM2[lower.tri(TPM2)] <- 0