library(tidyverse)
library(readxl)

# Run the benchmark model
system("gams model s=mdl --tax=yes")

# Run the scenarios
# Initialize the data frame
dat <- tibble()

# Loop over carbon tax and scc rates
for (co2p in c(0,25,50,75,100,150,200,250)) {
  for (scc in c(50,100,150,200,250)) {
    
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

ggplot(pd, aes(x=lk, y=nw, shape=policy, colour=scc, label=co2p, group=interaction(policy,scc))) +
  geom_line() +
  geom_point() 

ggplot(pd %>% filter(policy == "LSR"), aes(x=scc, y=co2p, fill=lk)) +
  geom_tile()
