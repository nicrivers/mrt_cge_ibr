library(tidyverse)
library(readxl)
library(kableExtra)
library(gt)

# First run the gams file that produces the data set
system("gams benchmark_table.gms")

dat <- read_excel("benchmark_table.xlsx", col_names = c("region", "item", "unit", "value"))

dat <- dat %>%
  unite(item_unit, item, unit, sep = " | ", remove = TRUE) %>% 
  pivot_wider(names_from = item_unit, values_from = value) %>%
  rename(
    `All sectors` = `Trade index | total`,
    EITE = `Trade index | EITE`,
    `CO2 intensity` = `CO2 intensity | (kg/$)`,
    `Fossil fuel net imports` = `Fossil fuel net imports | (% of GDP)`,
    `CO2` = `EITE | CO2 emissions (% of total)`,
    `Output` = `EITE | Output (% of total)`
  )


bt <- dat %>%
  gt() %>%
  fmt_number(
    decimals = 2
  ) %>%
  tab_spanner(
    label="Trade index",
    columns = c(`All sectors`,`EITE`)
  ) %>%
  tab_spanner(
    label="EITE share",
    columns = c(`CO2`,`Output`)
  ) 

bt


write_file(as_latex(bt) %>% as.character(), file="benchtable.tex")
