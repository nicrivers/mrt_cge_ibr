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

# lower case
dat <- dat %>%
  mutate(region_implementing = tolower(region_implementing),
         region = tolower(region))

# Decomposition of welfare from policy implementation
# terms of trade effect
tot <- dat %>%
  filter(item == "welfare$",
         scc == 250,
         region != region_implementing) %>%
  group_by(region_implementing,policy, co2p) %>%
  summarise(welfare_dollars = sum(value)) %>%
  arrange(region_implementing,policy, co2p) %>%
  group_by(region_implementing,policy) %>%
  mutate(tot_effect = - (welfare_dollars - first(welfare_dollars))) %>%
  dplyr::select(region_implementing,policy, co2p, tot_effect)

# leakage effect
leakage <- dat %>%
  filter(item == "emissions_abs", 
         scc == 250,
         region != region_implementing) %>%
  group_by(region_implementing,policy, co2p) %>%
  summarise(emissions = sum(value)) %>%
  group_by(region_implementing,policy) %>%
  arrange(region_implementing,policy, co2p) %>%
  mutate(welfare_leakage = -250 * (emissions - first(emissions))) %>%
  dplyr::select(region_implementing,policy, co2p, welfare_leakage)

# direct cost
direct_cost <- dat %>%
  filter(item == "welfare$",
         scc == 250,
         region == region_implementing) %>%
  group_by(region_implementing,policy, co2p) %>%
  summarise(welfare_dollars = sum(value)) %>%
  arrange(region_implementing,policy, co2p) %>%
  group_by(region_implementing,policy) %>%
  mutate(direct_cost = welfare_dollars - first(welfare_dollars)) %>%
  dplyr::select(region_implementing,policy, co2p, direct_cost) 
  
# domestic emission reduction benefit
dom_emit_benefit <- dat %>%
  filter(item == "emissions_abs", 
         scc == 250,
         region == region_implementing) %>%
  group_by(region_implementing,policy, co2p) %>%
  summarise(emissions = sum(value)) %>%
  group_by(region_implementing,policy) %>%
  arrange(region_implementing,policy, co2p) %>%
  mutate(welfare_emission_reduction = 250 * (first(emissions) - emissions)) %>%
  dplyr::select(region_implementing,policy, co2p, welfare_emission_reduction)

# total
decomp <- inner_join(
  tot, leakage
) %>%
  inner_join(direct_cost) %>%
  inner_join(dom_emit_benefit) %>%
  # Direct cost includes tot.  Remove
  mutate(direct_cost = direct_cost - tot_effect)

# Waterfall plot
decomp %>%
  rename(tot=tot_effect,
         dom_emit_benefit=welfare_emission_reduction,
         leakage=welfare_leakage) %>%
  mutate(total = dom_emit_benefit + direct_cost + leakage + tot) %>%
  pivot_longer(cols = tot:total) %>%
  mutate(name = factor(name, levels=c("dom_emit_benefit","direct_cost","leakage","tot","total"))) %>%
  filter(co2p %in% c(25,250)) %>%
  ggplot(aes(x=name, y=value, fill=policy)) +
  geom_col(position=position_dodge()) +
  facet_wrap(~co2p+region_implementing, scale = "free") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(name=NULL, guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name="Domestic welfare change (B$)", labels=scales::dollar_format()) +
  theme_light() 
ggsave("figures/welfare_decomp.png", width=8, height=4)  

# Net welfare
net_welfare <- dat %>% 
  filter(item == "netwelf") %>%
  dplyr::select(region_implementing,policy, nw=value, co2p, scc)
leakage <- dat %>% 
  filter(item == "Leakage", region == "all") %>%
  dplyr::select(region_implementing,policy, lk=value, co2p, scc)
pd <- inner_join(net_welfare, leakage) 




dat %>%
  filter(item %in% c("CO2 price", "netwelf"),
         sector == "all",
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=netwelf, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Change in domestic welfare (%)") +
  facet_wrap(~region_implementing)
#ggsave("figures/domestic_welfare.png", width=6, height=4)


dat %>%
  filter(item %in% c("CO2 price", "netwelf_globalscc"),
         sector == "all",
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=netwelf_globalscc, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Change in domestic welfare (%)") +
  facet_wrap(~ region_implementing)
ggsave("figures/domestic_welfare.png", width=6, height=4)

# Ranking table
dat %>%
  filter(item %in% c("CO2 price", "netwelf_globalscc"),
         sector == "all",
         region == region_implementing,
         #scc == 250
         ) %>%
  filter(scc >= co2p) %>%
  pivot_wider(names_from=item, values_from=value) %>% 
  group_by(region_implementing,co2p, scc) %>% 
  arrange(co2p, scc, desc(netwelf_globalscc)) %>% 
  summarise(best = first(policy), 
            second = nth(policy,2), 
            third = nth(policy,3), 
            fourth = nth(policy,4), 
            fifth = nth(policy, 5)) %>%
  ggplot(aes(x=scc, y=co2p, fill=best, label=best)) +
  geom_raster() +
  facet_wrap(~region_implementing) +
  scale_fill_brewer(name="policy",palette = "Set1", drop=FALSE) +
  theme_light() +
  scale_x_continuous(name="Social cost of carbon", labels=scales::dollar_format(suffix="/t")) +
  scale_y_continuous(name="Domestic carbon price", labels=scales::dollar_format(suffix="/t"))
ggsave("figures/optimal_policy.png", width=6, height=4)

# Leakage
pd %>%
  filter(scc == 250) %>%
  ggplot(aes(x=co2p, y=lk, colour=policy)) +
  facet_wrap(~region_implementing) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Leakage rate (%)")
ggsave("figures/leakage.png", width=6, height=4)


# Terms of trade
tot %>%
  ggplot(aes(x=co2p, y=tot_effect, colour=policy)) +
  facet_wrap(~region_implementing) +
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
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=Emissions, colour=policy)) +
  geom_line() +
  theme_light() +
  facet_wrap(~region_implementing) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Domestic emission reduction (%)")
ggsave("figures/domestic_emissions.png", width=6, height=4)


# Domestic emissions broken down to EITE vs. non-EITE
dat %>%
  filter(item %in% c("CO2 price", "Emissions"),
         sector %in% c("EITE", "non-EITE"),
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=Emissions, colour=policy)) +
  geom_line() +
  theme_light() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Domestic emission reduction (%)") +
  facet_wrap(~sector+region_implementing)
ggsave("figures/domestic_emissions_bysector.png", width=6, height=4)

# EITE output
dat %>%
  filter(item %in% c("Output"),
         sector == "EITE",
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=Output, colour=policy)) +
  geom_line() +
  theme_light() +
  facet_wrap(~region_implementing) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = "Domestic carbon price", labels=scales::dollar_format(suffix = "/t")) +
  scale_y_continuous(name = "Domestic EITE output (%)")
ggsave("figures/domestic_eite_output.png", width=6, height=4)

dat %>%
  filter(item %in% c("CO2 price", "eb(bn$)"),
         sector == "all",
         region == region_implementing,
         scc == 250) %>%
  pivot_wider(names_from=item, values_from=value) %>%
  ggplot(aes(x=co2p, y=`eb(bn$)`, colour=policy)) +
  geom_line() +
  facet_wrap(~region_implementing)




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


pd %>% filter(policy %in% c("A","O")) %>% 
  group_by(region_implementing,scc, co2p) %>%
  # Only keep where scc >= co2p
  filter(scc >= co2p) %>%
  select(-lk) %>%
  distinct() %>%
  pivot_wider(names_from = policy, values_from = nw) %>%
  mutate(diff = A-O) %>%
  ggplot(aes(x=scc, y=co2p, z=diff)) +
  geom_raster(aes(fill=diff)) +
  scale_fill_gradient2(name="Welfare\nABR-OBR") +
  geom_contour(breaks = c(-1,0,1), colour="black") +
  theme_light() +
  facet_wrap(~region_implementing) +
  scale_x_continuous(name="Social cost of carbon", labels=scales::dollar_format(suffix="/t")) +
  scale_y_continuous(name="Domestic carbon price", labels=scales::dollar_format(suffix="/t")) +
  annotate(geom="text", x=500,y=0, label="If SCC>>pCO2\nABR dominates", hjust=1, vjust=-0.5) +
  annotate(geom="text", x=500,y=325, label="If SCC<=pCO2\nOBR dominates", hjust=1) 
ggsave("figures/OBRvsABR.png", width=8, height=6)
                    