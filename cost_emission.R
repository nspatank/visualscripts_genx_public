#can only filter by case#

setwd("/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1")
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings="")
library(tidyverse)
library(ggrepel)


#don't divide if your load sums are already weighed correctly!


total_load <- read_csv("load_sums_weighed.csv") %>%
  mutate(demand = demand / 96)

directory <- na.omit(all_inputs$directory_name)

# create relevant folders 

dir.create(directory, showWarnings = FALSE)
dir.create((paste0(directory, "/cost_emissions")), showWarnings = FALSE)

Cost_csv <- read.csv(file="costs.csv", header=TRUE, sep=",")

cost <- subset(Cost_csv, (Costs=="cTotal"), select=c(Costs, Total, case, year))


ordered_set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])

#sunk costs
generators_csv <- read_csv("generators.csv")
capacity_csv <- read_csv("capacity.csv")

ordered_set <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""])

capacity <- select(capacity_csv, Resource, NewCap, NewEnergyCap, case, year) %>%
  filter(Resource != "Total")

generators_costs <- select( generators_csv, Resource,Inv_cost_per_MWyr, Inv_cost_per_MWhyr, case, year)
colnames(generators_costs)[1] <- "Resource.2"
colnames(generators_costs)[4] <- "case.2"
colnames(generators_costs)[5] <- "year.2"

joined_capacity_and_cost <- cbind(capacity, generators_costs) %>%
  filter(year != 2050) %>%
  mutate(added_build_cost = Inv_cost_per_MWyr * NewCap) %>%
  mutate(added_energy_cost = Inv_cost_per_MWhyr * NewEnergyCap)

totals <- tibble(case = NULL, year = NULL)
included_years <- na.omit(all_inputs$list_years[all_inputs$list_years!= ""])

for(i in 1:length(ordered_set)) {
  
  zero_row <- tibble(case = ordered_set[i], year = 2030, added = 0)
  
  first_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030)
  first_year_total <- sum(first_year$added_build_cost) + sum(first_year$added_energy_cost)
  first_row <- tibble(case = ordered_set[i], year = 2040, added = first_year_total)
  
  second_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2040)
  second_year_total <- sum(second_year$added_build_cost) + sum(second_year$added_energy_cost)
  second_row <- tibble(case = ordered_set[i], year = 2050, added = second_year_total)
  
  #obtain battery cost from 2030 because it has less than a 30 year life span
  battery_old_cost <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030 & Resource == "battery")
  battery_old_cost_total <- sum(battery_old_cost$added_build_cost) + sum(battery_old_cost$added_energy_cost)
  battery_old_cost_row <- tibble(case = ordered_set[i], year = 2050, added = battery_old_cost_total * -1)
  
  totals <- rbind(totals, zero_row, first_row, second_row, battery_old_cost_row)
}

totals <- aggregate(added~case+year, totals, sum)
  
totals <- group_by(totals, case) %>%
  mutate(csum = cumsum(added)) %>%
  ungroup() 


cost <- full_join(cost, totals) %>%
  mutate(Total = Total + csum)

# transmission
transmission_csv <- read_csv("trans.csv")
transmission <- mutate(transmission_csv, 
                       Cost_Trans_Capacity = ifelse(Cost_Trans_Capacity < 0, 0, Cost_Trans_Capacity))
transmission <- aggregate(Cost_Trans_Capacity~case+year, transmission, sum)

cost <- full_join(cost, transmission) %>%
  mutate(Total = Total + Cost_Trans_Capacity)


cost <- full_join(cost, total_load) %>%
  mutate(`$/MWh` = Total/demand)%>%
  filter(case %in% ordered_set)



cost$case <- factor(cost$case, levels = ordered_set) 

ggplot(cost , aes(x=case, y=`$/MWh`, fill = case))+
    geom_bar(stat="identity",width = 0.3) + 
    theme_bw()+ 
  facet_wrap(~year) +
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
    labs(x="cases", y="$/MWh") +
  ggsave(paste0(directory, "/cost_emissions/costs_all.png"), width=10, height=5, dpi=300)



emissions_csv <- read.csv(file="CO2.csv", header=TRUE, sep=",")
emissions <- subset(emissions_csv, (Zone=="Sum"), select=c(Zone, Total, case, year))
emissions <- left_join(emissions, total_load) %>%
  mutate(`CO2tons/MWh` = Total/demand)%>%
  filter(case %in% ordered_set)




emissions$case <- factor(emissions$case, levels = ordered_set)



ggplot(emissions , aes(x=case, y=`CO2tons/MWh`, fill = case))+
  geom_bar(stat="identity",width = 0.3) + 
  theme_bw()+ 
  facet_wrap(~year) +
  theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  labs(x="cases", y="CO2tons/MWh")+
  ggsave(paste0(directory, "/cost_emissions/emissions_all.png"), width=10, height=5, dpi=300)



joined <- full_join(cost, emissions, by = c("case", "year", "demand"))
joined$year <- as.factor(joined$year)


joined$year <- as.factor(joined$year)
ggplot(joined, aes(x = `CO2tons/MWh`, y = `$/MWh`, color = case, shape = year), size = 4) + geom_point() +
  labs(x="CO2tons/MWh", y="$/MWh")+
  ggsave(paste0(directory, "/cost_emissions/cost_by_emissions.png"), width=10, height=5, dpi=300)

