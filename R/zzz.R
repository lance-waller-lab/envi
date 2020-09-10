.onAttach <- function(...) {
  packageStartupMessage(paste("\nWelcome to {envi} ", utils::packageDescription("envi")$Version, "\n> help(\"envi\") # for documentation\n> citation(\"envi\") # for how to cite\n", sep = ""), appendLF = TRUE)
}
