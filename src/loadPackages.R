loadPackages <- function(packages) {
  for (package in (packages)) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package, dependencies = T)
      tryCatch(
        library(package, character.only=T),
        error=function(e)
          {message(paste0("erreur dans le chargement du package ", package))}
        )
    }
  }
}