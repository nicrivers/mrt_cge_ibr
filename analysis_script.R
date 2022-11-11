library(tidyverse)

dat <- read_csv("multi_scen.csv")

# Rename policies
dat <- dat %>%
  mutate(policy = case_when(policy == "LSR" ~ "LS",
                            policy == "OBR" ~ "O",
                            policy == "ABR" ~ "A",
                            policy == "ibr" ~ "IO",
                            policy == "bcr" ~ "IE"),
         policy = factor(policy, levels = c("LS", "O", "A", "IO", "IE")))

# Decomposition of welfare from policy implementation in the USA
# terms of trade effect
tot <- dat %>%
  filter(item == "welfare$",
         scc == 250,
         region != "usa") %>%
  group_by(policy, co2p) %>%
  summarise(welfare_dollars = sum(value)) %>%
  arrange(policy, co2p) %>%
  group_by(policy) %>%
  mutate(tot_effect_usa = - (welfare_dollars - first(welfare_dollars))) %>%
  dplyr::select(policy, co2p, tot_effect_usa)

# leakage effect
leakage <- dat %>%
  filter(item == "emissions_abs", 
         scc == 250,
         region != "usa") %>%
  group_by(policy, co2p) %>%
  summarise(emissions = sum(value)) %>%
  group_by(policy) %>%
  arrange(policy, co2p) %>%
  mutate(welfare_leakage = -250 * (emissions - first(emissions))) %>%
  dplyr::select(policy, co2p, welfare_leakage)

# direct cost
direct_cost <- dat %>%
  filter(item == "welfare$",
         scc == 250,
         region == "usa") %>%
  group_by(policy, co2p) %>%
  summarise(welfare_dollars = sum(value)) %>%
  arrange(policy, co2p) %>%
  group_by(policy) %>%
  mutate(direct_cost = welfare_dollars - first(welfare_dollars)) %>%
  dplyr::select(policy, co2p, direct_cost) 
  
# domestic emission reduction benefit
dom_emit_benefit <- dat %>%
  filter(item == "emissions_abs", 
         scc == 250,
         region == "usa") %>%
  group_by(policy, co2p) %>%
  summarise(emissions = sum(value)) %>%
  group_by(policy) %>%
  arrange(policy, co2p) %>%
  mutate(welfare_emission_reduction = 250 * (first(emissions) - emissions)) %>%
  dplyr::select(policy, co2p, welfare_emission_reduction)

# total
decomp <- inner_join(
  tot, leakage
) %>%
  inner_join(direct_cost) %>%
  inner_join(dom_emit_benefit) %>%
  # Direct cost includes tot.  Remove
  mutate(direct_cost = direct_cost - tot_effect_usa)

# Waterfall plot
decomp %>%
  rename(tot=tot_effect_usa,
         dom_emit_benefit=welfare_emission_reduction,
         leakage=welfare_leakage) %>%
  mutate(total = dom_emit_benefit + direct_cost + leakage + tot) %>%
  pivot_longer(cols = tot:total) %>%
  mutate(name = factor(name, levels=c("dom_emit_benefit","direct_cost","leakage","tot","total"))) %>%
  filter(co2p %in% c(25,250)) %>%
  ggplot(aes(x=name, y=value, fill=policy)) +
  geom_col(position=position_dodge()) +
  facet_wrap(~co2p) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(name=NULL, guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name="Domestic welfare change (B$)", labels=scales::dollar_format()) +
  theme_light()
ggsave("figures/welfare_decomp.png", width=6, height=4)  

# Net welfare
net_welfare <- dat %>% 
  filter(item == "netwelf") %>%
  dplyr::select(policy, nw=value, co2p, scc)
leakage <- dat %>% 
  filter(item == "Leakage", region == "all") %>%
  dplyr::select(policy, lk=value, co2p, scc)
pd <- inner_join(net_welfare, leakage) 




dat %>%
  filter(item %in% c("CO2 price", "netwelf"),
         sector == "all",
         region == "usa",
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=netwelf, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Change in domestic welfare (%)")
#ggsave("figures/domestic_welfare.png", width=6, height=4)


dat %>%
  filter(item %in% c("CO2 price", "netwelf_globalscc"),
         sector == "all",
         region == "usa",
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=netwelf_globalscc, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Change in domestic welfare (%)")
ggsave("figures/domestic_welfare.png", width=6, height=4)


# Leakage
pd %>%
  filter(scc == 250) %>%
  ggplot(aes(x=co2p, y=lk, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Leakage rate (%)")
ggsave("figures/leakage.png", width=6, height=4)


# Terms of trade
tot %>%
  ggplot(aes(x=co2p, y=tot_effect_usa, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Terms of trade impact (B$)", labels=scales::dollar_format())
ggsave("figures/tot.png", width=6, height=4)

# Domestic emissions
dat %>%
  filter(item %in% c("CO2 price", "Emissions"),
         sector == "all",
         region == "usa",
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=Emissions, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Domestic emission reduction (%)")
ggsave("figures/domestic_emissions.png", width=6, height=4)


dat %>%
  filter(item %in% c("CO2 price", "eb(bn$)"),
         sector == "all",
         region == "usa",
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=`eb(bn$)`, colour=policy)) +
  geom_line()




# Visualizing differences between policies
# ABR vs. OBR
pd %>% filter(policy %in% c("LS","O")) %>% 
  group_by(scc, co2p) %>%
  # Only keep where scc >= co2p
  filter(scc >= co2p) %>%
  select(-lk) %>%
  pivot_wider(names_from = policy, values_from = nw) %>%
  mutate(diff = LS-O) %>%
  ggplot(aes(x=scc, y=co2p, fill=diff)) +
  geom_tile() +
  scale_fill_fermenter()
