# Set your project folder here!!!!
setwd("C:/ASAR Project")

# Required package list
required.packages <- read.delim("packages", header = F, sep = "\n")
required.packages <- as.character(required.packages$V1)

# Currently installed packages
installed <- as.character(as.data.frame(installed.packages())$Package)

# Install require packages for Project
lapply(required.packages, FUN = function (package) {
  
  if(!package %in% installed) {
    print(paste("Installing:", package))
    install.packages(package, dependencies = T)
  } else {
    print(paste(package, "installed"))
  }
})

warnings()

update.packages()