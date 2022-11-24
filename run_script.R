library(tidyverse)
library(readxl)

# Run the benchmark model
system("gams model s=mdl --tax=yes")

# Run the scenarios
# Initialize the data frame
dat <- tibble()

# Loop over carbon tax and scc rates
for (co2p in seq(0,500,by=25)) {
  for (scc in seq(0,500,by=25)) {
    for (rg in c("USA","EUR")) {
    
    # Execute the model
    system(paste0("gams scen r=mdl --region=", rg," --co2p=",co2p," --scc=",scc))
    
    # Load the data
    dat_scen <- read_excel("single.xlsx", sheet="macro", skip=1, col_names = c("item", "sector", "region", "policy", "value"))
    
    # Add parameters
    dat_scen$co2p <- co2p
    dat_scen$scc <- scc
    dat_scen$region_implementing <- rg
    
    # Append to data frame
    dat <- bind_rows(dat, 
                     dat_scen)
    
    }
  }
}

write_csv(dat, "multi_scen.csv")
