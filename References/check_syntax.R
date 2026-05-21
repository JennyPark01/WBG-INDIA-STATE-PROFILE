tryCatch(
  {
    parse(file = "C:/Users/jaeye/Downloads/State profile/State profile_Lite.R")
    cat("SUCCESS: R file parses without errors\n")
  },
  error = function(e) {
    cat("PARSE ERROR:", e$message, "\n")
  }
)
