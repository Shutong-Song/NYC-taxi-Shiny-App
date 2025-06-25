my_packages = c("tidyverse","units","leaflet","leaflet.extras", "osrm", "shinyjs", "nycgeo", "sf", "geosphere")
install_if_missing = function(p) {
if (p %in% rownames(installed.packages()) == FALSE) {
install.packages(p)
}
}
invisible(sapply(my_packages, install_if_missing))
