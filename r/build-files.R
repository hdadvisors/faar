library(tidyverse)

locals <- c("Fredericksburg", "Caroline", "King George",
            "Orange", "Spotsylvania", "Stafford")

abbr <- c("fred", "caro", "kg",
          "orng", "spot", "staf")

file_names <- paste0("local-", abbr, ".qmd")
  
titles <- paste(locals, "fact sheet")

create_qmd_file <- function(file_name, title) {
  content <- paste("# ", title, "\n", "\n", "...", sep = "")
    write(content, file_name)
}

map2(file_names, titles, create_qmd_file)
