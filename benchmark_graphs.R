library(tidyverse)
library(readxl)
library(kableExtra)
library(gt)

gdp <- read_excel("results_benchmark.xlsx", sheet = "gdp")
co2 <- read_excel("results_benchmark.xlsx", sheet = "co2")
energy <- read_excel("results_benchmark.xlsx", sheet = "energy")
trdshr <- read_excel("results_benchmark.xlsx", sheet = "trdshr")
trdindex <- read_excel("results_benchmark.xlsx", sheet = "trdindex")
ecm <- read_excel("results_benchmark.xlsx", sheet = "ecm")
co2int <- read_excel("results_benchmark.xlsx", sheet = "co2int")
fig1 <- read_excel("results_benchmark.xlsx", sheet = "fig1")
fig2 <- read_excel("results_benchmark.xlsx", sheet = "fig2")
fig3 <- read_excel("results_benchmark.xlsx", sheet = "fig3")
fig4 <- read_excel("results_benchmark.xlsx", sheet = "fig4")

ggplot(co2int %>% filter(sector=="all_gdp"), aes(x=region, y=value)) +
  geom_col()

ggplot(fig1 %>% filter(fmt == "adj"), aes(x=region, y=value)) +
  geom_col() +
  facet_wrap(~sector, scales="free") +
  coord_flip() +
  labs(x="",
       y="Carbon content (kgCO2 per US$)")

fig2 <- fig2 %>% mutate(item=factor(item, c("imported","indirect", "direct", "total"), ordered=TRUE))
ggplot(fig2 %>% filter(fmt == "adj", item != "total", region %in% c("USA","EUR")), aes(x=region, y=value, fill=item, alpha=item)) +
  geom_col() +
  facet_wrap(~ sector, scales = "free") +
#  coord_flip() +
  labs(x="",
       y="Carbon content (kgCO2 per US$)") +
  theme_bw() +
  scale_fill_discrete(name="") +
  scale_alpha_manual(name="", values=c("direct"=1,"indirect"=1,"imported"=0.5), guide="none")
ggsave("figures/sector_intensity_compare.png", width=8, height=6)

ggplot(fig3 %>% filter(fmt == "adj", item != "net") %>% pivot_wider(names_from=item, values_from = value), aes(x=exports, y=imports, colour=region)) +
  geom_abline(slope=1, intercept=0) +
  theme_bw() +
  geom_text(aes(label=region))

dat <-
  inner_join(
    co2int %>% filter(sector == "all_gdp") %>% select(region, co2_intensity=value),
    trdindex %>% filter(item == "TII") %>% select(region, trade_index=value)
  ) %>%
  inner_join(
    co2 %>% filter(sector == "all") %>% select(-sector, co2=value)
  ) %>%
  inner_join(
    energy %>% filter(fmt == "EJ") %>% group_by(region, item) %>% summarise(value = sum(value)) %>% pivot_wider(names_from=item, values_from=value) %>% mutate(net_exports = exports - imports) %>% select(region, production, net_exports)
  )

bt <- dat %>%
  gt() %>%
  fmt_number(
    columns = co2_intensity:trade_index,
    decimals = 2
  ) %>%
  fmt_number(
    columns = co2:net_exports,
    decimals=0
  ) %>%
  tab_spanner(
    label="Fossil fuel (EJ)",
    columns = c("production","net_exports")
  ) %>%
  cols_label(
    co2_intensity = "CO2 intensity kg/$",
    trade_index = "Trade index",
    co2 = "CO2 (Mt)",
    production = "Production",
    net_exports = "Net exports"
  ) 

bt


write_file(as_latex(bt) %>% as.character(), file="benchtable.tex")
