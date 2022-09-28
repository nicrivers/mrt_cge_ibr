library(tidyverse)
library(readxl)

# Run the benchmark model
system("gams model s=mdl --tax=yes")

# Run the scenarios
# Initialize the data frame
dat <- tibble()

# Loop over carbon tax and scc rates
for (co2p in seq(0,100,by=50)) {
  for (scc in seq(50,250,by=100)) {
    
    # Execute the model
    system(paste0("gams scen r=mdl --region=USA --co2p=",co2p," --scc=",scc))
    
    # Load the data
    dat_scen <- read_excel("single.xlsx", sheet="macro", skip=1, col_names = c("item", "sector", "region", "policy", "value"))
    
    # Add parameters
    dat_scen$co2p <- co2p
    dat_scen$scc <- scc
    
    # Append to data frame
    dat <- bind_rows(dat, 
                     dat_scen)
    
  }
}

# Net welfare
net_welfare <- dat %>% 
  filter(item == "netwelf") %>%
  dplyr::select(policy, nw=value, co2p, scc)
leakage <- dat %>% 
  filter(item == "Leakage", region == "all") %>%
  dplyr::select(policy, lk=value, co2p, scc)
pd <- inner_join(net_welfare, leakage)

ggplot(pd, aes(x=lk, y=nw, shape=policy, colour=scc)) +
  geom_point() +
  geom_line()

ggplot(pd %>% filter(policy == "LSR"), aes(x=scc, y=co2p, fill=nw)) +
  geom_raster()
