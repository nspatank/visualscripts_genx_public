#---0.ADMIN - Include libraries and other important-----------
setwd("/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1")
path <- "/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1"
outpath <- "/Users/anikamaskara/Desktop/P2X/genx_viz/PJM_results_Sep1"
##--A. Clean

#Remove only selected variables if wanted, otherwise remove all
# rm(list = setdiff ( ls() , "") )

#Create basic plot, assign output to a variable and then remove variable

##--B.Load Library for county manipulations
#suppressMessages ( library ( "ggmap"       , lib.loc=.libPaths() ) )      # mapping functions, ggmap()
suppressMessages ( library ( "ggplot2"     , lib.loc=.libPaths() ) )      # plots, ggplot()
#suppressMessages ( library ( "maps"        , lib.loc=.libPaths() ) )      # map_data()
suppressMessages ( library ( "RColorBrewer", lib.loc=.libPaths() ) )      # color pallettes
suppressMessages ( library ( "ggrepel", lib.loc=.libPaths() ) )           # For plot label placemente
#suppressMessages ( library ( "rgdal", lib.loc=.libPaths() ) )             # geospatial layers

# Script to gather a set of GenX case results from a directory and compile into data.frames
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(RSQLite)
library(gridExtra)
library(dplyr)
library(reshape)
# source('./SB100_plots_vPJM.R')


##################
# Primary Compilation Script

all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE, sep=",")


# Lists all files
allfiles = list.files(recursive = TRUE)

years = na.omit(all_inputs$list_years)
demand_years <- vector(mode = "list", length= length(years))
weights_years <- vector(mode = "list", length= length(years))

for (i in 1:length(years)){
  # Load demand inputs
  grep_load = paste0(years[i], ".*Load_data.csv|", years[i], ".*Load_data.csv")
  demand_years[[i]] <- read.csv(paste0(path, "/", grep(grep_load,allfiles, value=TRUE)[1]))
  demand_years[[i]]$year <- years[i]
  
  # Load hourly weights
  grep_weight = paste0(years[i], ".*time_weights.csv|", years[i], ".*time_weights.csv")
  weights_years[[i]]<- read.csv(paste0(path, "/", grep(grep_weight,allfiles, value=TRUE)[1]))
  demand_years[[i]]$weight <- weights_years[[i]]$Weight
}

# Extract hourly zonal time series and bind demand dataframes
demand <- demand_years[[1]][,8:ncol(demand_years[[1]])]
for (i in 2:length(years))
{
  demand <- bind_rows(demand, demand_years[[i]][,8:ncol(demand_years[[i]])])
}


capacities = allfiles[grep("/capacity.csv", allfiles)]

find_case = which(strsplit(capacities[[1]], "")[[1]]=="/")
find_year = which(strsplit(capacities[[1]], "")[[1]]=="_")

# Extracts all relevant csv files
all_costs = allfiles[grep("/costs.csv", allfiles)]
all_trans = allfiles[grep("/network_expansion.csv", allfiles)]
all_flows = allfiles[grep("/flow.csv", allfiles)]
all_power = allfiles[grep("/power.csv", allfiles)]
all_charge = allfiles[grep("/charge.csv", allfiles)]
all_nse = allfiles[grep("/nse.csv", allfiles)]
all_CO2 = allfiles[grep("/emissions.csv", allfiles)]
all_RPS_CES = allfiles[grep("/RPS.csv", allfiles)]
all_prices = allfiles[grep("/prices.csv", allfiles)]

all_generators_data = allfiles[grep("/Generators_data.csv", allfiles)]

# Creates primary instance of compiled files
capacity = read.csv(paste0(path,"/",capacities[[1]]))
capacity$case = substr(capacities[[1]], find_year[2] + 1, find_case[2] - 1)
capacity$year = substr(capacities[[1]],1, 4)

costs = read.csv(paste0(path,"/",all_costs[[1]]))
costs$case = substr(all_costs[[1]], find_year[2] + 1, find_case[2] - 1)
costs$year = substr(all_costs[[1]],1, 4)

trans = read.csv(paste0(path,"/",all_trans[[1]]))
trans$case = substr(all_trans[[1]], find_year[2] + 1, find_case[2] - 1)
trans$year = substr(all_trans[[1]],1, 4)

flows = read.csv(paste0(path,"/",all_flows[[1]]))
flows$case = substr(all_flows[[1]], find_year[2] + 1, find_case[2] - 1)
flows$year = substr(all_flows[[1]],1, 4)

power = read.csv(paste0(path,"/",all_power[[1]]))
power$case = substr(all_power[[1]], find_year[2] + 1, find_case[2] - 1)
power$year = substr(all_power[[1]],1, 4)

charge = read.csv(paste0(path,"/",all_charge[[1]]))
charge$case = substr(all_charge[[1]], find_year[2] + 1, find_case[2] - 1)
charge$year = substr(all_charge[[1]],1, 4)

nse = read.csv(paste0(path,"/",all_nse[[1]]))
nse$case = substr(all_nse[[1]], find_year[2] + 1, find_case[2] - 1)
nse$year = substr(all_nse[[1]],1, 4)

CO2 = read.csv(paste0(path,"/",all_CO2[[1]]))
CO2$case = substr(all_CO2[[1]], find_year[2] + 1, find_case[2] - 1)
CO2$year = substr(all_CO2[[1]],1, 4)

RPS_CES = read.csv(paste0(path,"/",all_RPS_CES[[1]]))
RPS_CES$case = substr(all_RPS_CES[[1]], find_year[2] + 1, find_case[2] - 1)
RPS_CES$year = substr(all_RPS_CES[[1]],1, 4)

generators = read.csv(paste0(path,"/",all_generators_data[[1]]))
generators$case = substr(all_generators_data[[1]], find_year[2] + 1, find_case[2] - 1)
generators$year = substr(all_generators_data[[1]],1, 4)

if(length(all_prices>0)){
  prices = read.csv(paste0(path,"/",all_prices[[1]]))
  prices$case = substr(all_prices[[1]], find_year[2] + 1, find_case[2] - 1)
  prices$year = substr(all_prices[[1]],1, 4)
}

# Binds all remaining csv files together 
for(f in 2:length(capacities)){
  temp_capacity = read.csv(paste0(path,"/",capacities[[f]]))
  temp_capacity$case = substr(capacities[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_capacity$year = substr(capacities[[f]],1, 4)
  
  temp_costs = read.csv(paste0(path,"/",all_costs[[f]]))
  temp_costs$case = substr(all_costs[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_costs$year = substr(all_costs[[f]],1, 4)
  
  temp_trans = read.csv(paste0(path,"/",all_trans[[f]]))
  temp_trans$case = substr(all_trans[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_trans$year = substr(all_trans[[f]],1, 4)
  
  temp_flows = read.csv(paste0(path,"/",all_flows[[f]]))
  temp_flows$case = substr(all_flows[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_flows$year = substr(all_flows[[f]],1, 4)
  
  temp_power = read.csv(paste0(path,"/",all_power[[f]]))
  temp_power$case = substr(all_power[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_power$year = substr(all_power[[f]],1, 4)
  
  temp_charge = read.csv(paste0(path,"/",all_charge[[f]]))
  temp_charge$case = substr(all_charge[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_charge$year = substr(all_charge[[f]],1, 4)
  
  temp_nse = read.csv(paste0(path,"/",all_nse[[f]]))
  temp_nse$case = substr(all_nse[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_nse$year = substr(all_nse[[f]],1, 4)
  
  temp_CO2 = read.csv(paste0(path,"/",all_CO2[[f]]))
  temp_CO2$case = substr(all_CO2[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_CO2$year = substr(all_CO2[[f]],1, 4)
  
  temp_RPS_CES = read.csv(paste0(path,"/",all_RPS_CES[[f]]))
  temp_RPS_CES$case = substr(all_RPS_CES[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_RPS_CES$year = substr(all_RPS_CES[[f]],1, 4)
  
  temp_generators = read.csv(paste0(path,"/",all_generators_data[[f]]))
  temp_generators$case = substr(all_generators_data[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
  temp_generators$year = substr(all_generators_data[[f]],1, 4)
  
  
  if(!is.na(all_prices[f])){
    temp_prices = read.csv(paste0(path,"/",all_prices[[f]]))
    temp_prices$case = substr(all_prices[[f]], find_year[2] + 1, which(strsplit(capacities[[f]], "")[[1]]=="/")[2] - 1)
    temp_prices$year = substr(all_prices[[f]],1, 4)
  }
  
  capacity <- bind_rows(capacity, temp_capacity)
  costs <- bind_rows(costs, temp_costs)
  trans <- bind_rows(trans, temp_trans)
  flows <- bind_rows(flows, temp_flows)
  power <- bind_rows(power, temp_power)
  charge <- bind_rows(charge, temp_charge)
  nse <- bind_rows(nse, temp_nse)
  CO2 <- bind_rows(CO2, temp_CO2)
  RPS_CES <- bind_rows(RPS_CES, temp_RPS_CES)
  generators <- bind_rows(generators, temp_generators)
  if(!is.na(all_prices[f])){
    prices <- bind_rows(prices, temp_prices)
  }
}

sep = "/"

write.csv(demand, paste0(outpath, sep, "demand.csv"), row.names = FALSE)
write.csv(capacity, paste0(outpath, sep, "capacity.csv"), row.names = FALSE)
write.csv(costs, paste0(outpath, sep, "costs.csv"), row.names = FALSE)
write.csv(trans, paste0(outpath, sep, "trans.csv"), row.names = FALSE)
write.csv(flows, paste0(outpath, sep, "flows.csv"), row.names = FALSE)
write.csv(power, paste0(outpath, sep, "power.csv"), row.names = FALSE)
write.csv(charge, paste0(outpath, sep, "charge.csv"), row.names = FALSE)
write.csv(nse, paste0(outpath, sep, "nse.csv"), row.names = FALSE)
write.csv(CO2, paste0(outpath, sep, "CO2.csv"), row.names = FALSE)
write.csv(RPS_CES, paste0(outpath, sep, "RPS_CES.csv"), row.names = FALSE)
write.csv(prices, paste0(outpath, sep, "prices.csv"), row.names = FALSE)
write.csv(generators, paste0(outpath, sep, "generators.csv"), row.names = FALSE)


###################################################################################
# - Plots


# Run SB100 Plots (disabled until it works)
# for (i in 1:length(years)){
#   SB100_plots(years[i], demand_years[[i]])
# }




