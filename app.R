# When run via Rscript, set working directory to the script's own folder
local({
  flag <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(flag)) setwd(dirname(normalizePath(sub("--file=", "", flag))))
})

options(shiny.launch.browser = TRUE)
# print.eval = TRUE forces the shinyApp() object to be printed,
# which triggers runApp() even in non-interactive Rscript sessions
source("02_shiny_app.R", print.eval = TRUE)

