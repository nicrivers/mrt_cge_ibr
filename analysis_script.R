library(tidyverse)

dat <- read_csv("multi_scen.csv")


# Net welfare
net_welfare <- dat %>% 
  filter(item == "netwelf") %>%
  dplyr::select(policy, nw=value, co2p, scc)
leakage <- dat %>% 
  filter(item == "Leakage", region == "all") %>%
  dplyr::select(policy, lk=value, co2p, scc)
pd <- inner_join(net_welfare, leakage) 

ggplot(pd, aes(x=lk, y=nw, shape=policy, colour=factor(scc), group=interaction(policy,scc))) +
  geom_path() +
  geom_point() 

ggplot(pd %>% filter(scc == 250), aes(x=lk, y=nw, shape=policy, colour=factor(co2p), group=policy)) +
  geom_path() +
  geom_point() +
  theme_bw() 
  


# Visualizing differences between policies
# LSR vs. OBR
pd %>% filter(policy %in% c("LSR","OBR")) %>% 
  group_by(scc, co2p) %>%
  # Only keep where scc >= co2p
  filter(scc >= co2p) %>%
  select(-lk) %>%
  pivot_wider(names_from = policy, values_from = nw) %>%
  mutate(diff = LSR-OBR) %>%
  ggplot(aes(x=scc, y=co2p, fill=diff)) +
  geom_tile() +
  scale_fill_fermenter()

# OBR vs. IBR
pd %>% filter(policy %in% c("OBR","ibr")) %>% 
  group_by(scc, co2p) %>%
  # Only keep where scc >= co2p
  filter(scc >= co2p) %>%
  select(-lk) %>%
  pivot_wider(names_from = policy, values_from = nw) %>%
  mutate(diff = ibr-OBR) %>%
  ggplot(aes(x=scc, y=co2p, fill=diff)) +
  geom_tile() +
  scale_fill_fermenter()

# LSR vs. IBR
pd %>% filter(policy %in% c("LSR","ibr")) %>% 
  group_by(scc, co2p) %>%
  # Only keep where scc >= co2p
  filter(scc >= co2p) %>%
  select(-lk) %>%
  pivot_wider(names_from = policy, values_from = nw) %>%
  mutate(diff = LSR-ibr) %>%
  ggplot(aes(x=scc, y=co2p, fill=diff)) +
  geom_tile() +
  scale_fill_fermenter()

ggplot(pd %>% filter(scc==0), aes(x=co2p, y=nw, shape=policy, colour=factor(scc), label=co2p, group=interaction(policy,scc))) +
  geom_path() +
  geom_point() 
