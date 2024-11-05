# Function to install and load multiple R packages
setup_packages <- function(pkg_list) {
  # Check which packages are not installed
  new_packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
  
  # Install missing packages
  if(length(new_packages) > 0) {
    message("Installing these packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  }
  
  # Load all packages
  invisible(lapply(pkg_list, library, character.only = TRUE))
  
  message("All packages have been installed and loaded!")
}

# List of your commonly used packages
packages <- c(
  "tidyverse",    # Data manipulation and visualization
  "tidycensus",   # Census data access
  "janitor",      # Data cleaning
  "FinCal",       # Financial calculations
  "scales",       # Better plot scaling
  "kableExtra",   # Enhanced table formatting
  "readxl",       # Excel file reading
  "hdatools",     # HDA specific tools
  "ggtext",       # Text formatting in ggplot
  "ggiraph"       # Interactive ggplot graphics
)

# Run the setup
setup_packages(packages)

