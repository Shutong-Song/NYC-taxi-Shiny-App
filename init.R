install.packages("remotes")
remotes::install_github("mfherman/nycgeo")
my_packages = c("tidyverse","leaflet","leaflet.extras", "osrm", "shinyjs", "sf", "geosphere")
install_if_missing = function(p) {
if (p %in% rownames(installed.packages()) == FALSE) {
install.packages(p)
}
}
invisible(sapply(my_packages, install_if_missing))
