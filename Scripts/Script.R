
#Load library

# Package names
packages <- c("ggplot2", "here", "tidyverse","runjags","ggplot2","ggridges","readxl","ggpmisc")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Read in the shapefile
database <- read_excel(here("Data","Database.xlsx"))


#Data has one col for each test and one for population, see the exemple:

data<- data.frame(Population = database$Population,
                  
                  WipeResult = database$WipeResult,
                  
                  SwabResult = database$SwabResult
                  
)

# Acquire data and priors

# template_huiwalter(data, outfile=here("Database","huiwalter_model.txt"), covariance=FALSE,
#                    se_priors = "dbeta(2,1)",
#                    sp_priors = "dbeta(2,1)")



## Run the model

results <- run.jags(here("Data","huiwalter_model.txt"),
                    burnin = 15000,
                    sample = 30000,
                    n.chains = 4,
                    thin = 3)

