
 
dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/cost_emissions", showWarnings = FALSE)

Cost_csv <- read.csv(file="costs.csv", header=TRUE, sep=",")

cost <- subset(Cost_csv, (Costs=="cTotal"), select=c(Costs, Total, case, year))

#sunk costs
generators_csv <- read_csv("generators.csv")
generators_costs <- select( generators_csv, region, Resource,zone, cluster, Inv_cost_per_MWyr, Inv_cost_per_MWhyr, case, year)

capacity_csv <- read_csv("capacity.csv")
capacity <- select(capacity_csv, Region, Resource, Zone, Cluster, NewCap, NewEnergyCap, case, year) %>%
  filter(Resource != "n/a",year != 2020)#it was total but total is no longer  there
capacity$Zone <- as.numeric(capacity$Zone);
capacity$Cluster <- as.numeric(capacity$Cluster);
joined_capacity_and_cost <- merge(capacity, generators_costs,by.x = c("Region", "Resource","Zone","Cluster", "case","year"),by.y = c("region", "Resource","zone","cluster", "case","year")) %>%
  #filter(year != 2050) %>%
  mutate(added_build_cost = Inv_cost_per_MWyr * NewCap) %>%
  mutate(added_energy_cost = Inv_cost_per_MWhyr * NewEnergyCap)

totals <- tibble(case = NULL, year = NULL)
if (years[1] == 2022){
  for(i in 1:length(ordered_set)) {
    zero_row <- tibble(case = ordered_set[i], year = 2022, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2022)
    first_year_total <- sum(first_year$added_build_cost) + sum(first_year$added_energy_cost)
    first_row <- tibble(case = ordered_set[i], year = 2025, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2025)
    second_year_total <- sum(second_year$added_build_cost) + sum(second_year$added_energy_cost)
    second_row <- tibble(case = ordered_set[i], year = 2030, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2022)
    third_year_total <- sum(third_year$added_build_cost) + sum(third_year$added_energy_cost)
    third_row <- tibble(case = ordered_set[i], year = 2030, added = third_year_total)  
    #obtain battery cost from 2030 because it has less than a 30 year life span
    # battery_old_cost <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030 & Resource == "battery_mid")
    # battery_old_cost_total <- sum(battery_old_cost$added_build_cost) + sum(battery_old_cost$added_energy_cost)
    # battery_old_cost_row <- tibble(case = ordered_set[i], year = 2050, added = battery_old_cost_total * -1)
    totals <- rbind(totals, zero_row, first_row, second_row, third_row, battery_old_cost_row)
  }
} else if (years[1] == 2030){
  for(i in 1:length(ordered_set)) {
    zero_row <- tibble(case = ordered_set[i], year = 2030, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030)
    first_year_total <- sum(first_year$added_build_cost) + sum(first_year$added_energy_cost)
    first_row <- tibble(case = ordered_set[i], year = 2040, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2040)
    second_year_total <- sum(second_year$added_build_cost) + sum(second_year$added_energy_cost)
    second_row <- tibble(case = ordered_set[i], year = 2050, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030)
    third_year_total <- sum(third_year$added_build_cost) + sum(third_year$added_energy_cost)
    third_row <- tibble(case = ordered_set[i], year = 2050, added = third_year_total)  
    #obtain battery cost from 2030 because it has less than a 30 year life span
    battery_old_cost <- filter(joined_capacity_and_cost, case == ordered_set[i] & year == 2030 & Resource == "battery_mid")
    battery_old_cost_total <- sum(battery_old_cost$added_build_cost) + sum(battery_old_cost$added_energy_cost)
    battery_old_cost_row <- tibble(case = ordered_set[i], year = 2050, added = battery_old_cost_total * -1)
    totals <- rbind(totals, zero_row, first_row, second_row, third_row, battery_old_cost_row)
  }
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

totals <- tibble(case = NULL, year = NULL)
if (years[1] == 2022) {
  for(i in 1:length(ordered_set)) {
    zero_row <- tibble(case = ordered_set[i], year = 2022, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(transmission, case == ordered_set[i] & year == 2022)
    first_year_total <- sum(first_year$Cost_Trans_Capacity)
    first_row <- tibble(case = ordered_set[i], year = 2025, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(transmission, case == ordered_set[i] & year == 2025)
    second_year_total <- sum(second_year$Cost_Trans_Capacity)
    second_row <- tibble(case = ordered_set[i], year = 2030, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(transmission, case == ordered_set[i] & year == 2022)
    third_year_total <- sum(third_year$Cost_Trans_Capacity)
    third_row <- tibble(case = ordered_set[i], year = 2030, added = third_year_total) 
    totals <- rbind(totals, zero_row, first_row, second_row, third_row)
  }
  } else if (years[1] == 2030){
    for(i in 1:length(ordered_set)) {
      zero_row <- tibble(case = ordered_set[i], year = 2030, added = 0)
      # adding 2030 exp cost to 2040
      first_year <- filter(transmission, case == ordered_set[i] & year == 2030)
      first_year_total <- sum(first_year$Cost_Trans_Capacity)
      first_row <- tibble(case = ordered_set[i], year = 2040, added = first_year_total)
      # adding 2040 exp cost to 2050
      second_year <- filter(transmission, case == ordered_set[i] & year == 2040)
      second_year_total <- sum(second_year$Cost_Trans_Capacity)
      second_row <- tibble(case = ordered_set[i], year = 2050, added = second_year_total)
      # adding 2030 exp cost to 2050
      third_year <- filter(transmission, case == ordered_set[i] & year == 2030)
      third_year_total <- sum(third_year$Cost_Trans_Capacity)
      third_row <- tibble(case = ordered_set[i], year = 2050, added = third_year_total) 
      totals <- rbind(totals, zero_row, first_row, second_row, third_row)
    }
}


totals <- aggregate(added~case+year, totals, sum)

totals <- group_by(totals, case) %>%
  mutate(csumT = cumsum(added)) %>%
  ungroup() 

cost <- merge(cost, totals,by=c("case","year")) %>%
  mutate(Total = Total + csumT)
# cost <- full_join(cost, transmission) %>%
#   mutate(Total = Total + Cost_Trans_Capacity)
cost$year <- as.factor(cost$year);
cost <- full_join(cost, total_load) %>%
  mutate(`$/MWh` = Total/demand)
cost$case <- factor(cost$case, levels = ordered_set)
ggplot(cost , aes(x=case, y=`$/MWh`, fill = case))+
    geom_bar(stat="identity",width = 0.3) + 
    theme_bw()+ 
  facet_wrap(~year) +
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
    labs(x="cases", y="$/MWh") +
  ggsave("Graphics/cost_emissions/costs_all.png", width=10, height=5, dpi=300)

# plotting the emissions and cost--------

emissions_csv <- read.csv(file="CO2.csv", header=TRUE, sep=",")
emissions <- subset(emissions_csv, (Zone=="Sum"), select=c(Zone, Total, case, year))
emissions$year <- as.factor(emissions$year);
emissions <- left_join(emissions, total_load) %>%
  mutate(`CO2tons/MWh` = Total/demand)

emissions$case <- factor(emissions$case, levels = ordered_set)




ggplot(emissions , aes(x=case, y=`CO2tons/MWh`, fill = case))+
  geom_bar(stat="identity",width = 0.3) + 
  theme_bw()+ 
  facet_wrap(~year) +
  theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  labs(x="cases", y="CO2tons/MWh")+
  ggsave("Graphics/cost_emissions/emissions_all.png", width=10, height=5, dpi=300)


colnames(cost)[2] <- "Cost"
colnames(emissions)[2] <- "Emission"

joined <- full_join(cost, emissions)
joined$year <- as.factor(joined$year)
ggplot(joined, aes(x = `CO2tons/MWh`, y = `$/MWh`, color = case, shape = year)) + geom_point(size = 3) +
  labs(x="CO2tons/MWh", y="$/MWh")+
  theme_bw()+
  ggsave("Graphics/cost_emissions/cost_by_emissions.png", width=10, height=5, dpi=300)

