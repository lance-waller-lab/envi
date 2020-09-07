.onAttach <- function(...) {
  packageStartupMessage(paste("\nWelcome to {envi} ", packageDescription("envi")$Version, "\n- type help(\"envi\") for documentation\n- type citation(\"envi\") for how to cite\n", sep = ""), appendLF = TRUE)
}
