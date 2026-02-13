# Suppress R CMD check NOTEs for non-standard evaluation (NSE)
# Used by data.table and ggplot2
utils::globalVariables(c(
  # ggplot2 aesthetics
  "value",
  "component", 
  "label",
  
  # data.table variables
  "series",
  "lambda",
  
  # magrittr pipe
  "."
))
