all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE,na.strings="")
zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)
##################

sep = "/"
# Lists all files
allfiles = list.files(recursive = TRUE)
#This is to find where the "case" is in the string
find_case = which(strsplit(allfiles[[1]], "")[[1]]=="/") 
#This is to find where the "year" is in the string
find_year = which(strsplit(allfiles[[1]], "")[[1]]=="_")

#-----------------------------------------#
#        Combining Demand data            #
#-----------------------------------------#
#note that load_data and time_weights must be in pairs
all_demand = allfiles[grep("/Load_data.csv", allfiles)]
all_time_weight = allfiles[grep("/time_weights.csv", allfiles)]
demand = read.csv(paste0(path,"/",all_demand[[1]]))
demand$case = substr(all_demand[[1]], find_year[2] + 1, find_case[2] - 1)
demand$year = substr(all_demand[[1]],1, 4)
time_weight = read.csv(paste0(path,"/",all_time_weight[[1]]))
demand$weight = time_weight$Weight
if (length(all_demand)>1)
{
  for ( f in 2:length(all_demand))
  {
    temp_demand = read.csv(paste0(path,"/",all_demand[[f]]))
    temp_demand$case = substr(all_demand[[f]], find_year[2] + 1, which(strsplit(all_demand[[f]], "")[[1]]=="/")[2] - 1)
    temp_demand$year = substr(all_demand[[f]],1, 4)
    temp_time_weight = read.csv(paste0(path,"/",all_time_weight[[f]]))
    temp_demand$weight = time_weight$Weight
    demand <- bind_rows(demand, temp_demand)
  }
}
total_demand <- demand[,9:ncol(demand)];
total_demand$systemload <- rowSums(total_demand[,2:(ncol(total_demand)-3)]);
total_demand$systemload_weighted <- (total_demand$systemload)*(total_demand$weight);

total_load <- aggregate(systemload_weighted~case+year,total_demand,sum) %>%
  plyr::rename(c("systemload_weighted" = "demand"));


write.csv(demand, paste0(outpath, sep, "demand.csv"), row.names = FALSE)
write.csv(total_load, paste0(outpath, sep, "load_sums_weighted.csv"), row.names = FALSE)



#-------------------------------#
#     Combining capacity data   #
#-------------------------------#
all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE,na.strings="")
zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)
##################

sep = "/"
# Lists all files
allfiles = list.files(recursive = TRUE)
#This is to find where the "case" is in the string
find_case = which(strsplit(allfiles[[1]], "")[[1]]=="/") 
#This is to find where the "year" is in the string
find_year = which(strsplit(allfiles[[1]], "")[[1]]=="_")
#Readfilepath
all_generators_data <- allfiles[grep("/Generators_data.csv", allfiles)];
all_capacity_data <- allfiles[grep("/capacity.csv", allfiles)];

capacity <- read.csv(paste0(path,"/",all_capacity_data[[1]]));
capacity <- capacity[-nrow(capacity),];
t_generators <- read.csv(paste0(path,"/",all_generators_data[[1]]));
capacity <- cbind(capacity,t_generators$Fuel);
colnames(capacity)[dim(capacity)[2]] <- "Fuel";
capacity$case <- substr(all_capacity_data[[1]], find_year[2] + 1, find_case[2] - 1)
capacity$year <- substr(all_capacity_data[[1]],1, 4)
if (length(all_capacity_data)>1)
{
  for(f in 2:length(all_capacity_data))
  {
    temp_capacity <- read.csv(paste0(path,"/",all_capacity_data[[f]]));
    temp_capacity <- temp_capacity[-nrow(temp_capacity),];
    t_generators <- read.csv(paste0(path,"/",all_generators_data[[f]]));
    temp_capacity <- cbind(temp_capacity,t_generators$Fuel);
    colnames(temp_capacity)[dim(temp_capacity)[2]] <- "Fuel";
    temp_capacity$case = substr(all_capacity_data[[f]], find_year[2] + 1, which(strsplit(all_capacity_data[[f]], "")[[1]]=="/")[2] - 1)
    temp_capacity$year = substr(all_capacity_data[[f]],1, 4)
    capacity <- bind_rows(capacity, temp_capacity)
  }
}

capacity$Zone <-as.numeric(capacity$Zone);
capacity$Cluster <-as.numeric(capacity$Cluster);
#set up the first year data
temp_capacity <- subset(capacity,year == years[1]) %>%
  mutate(EndCap = StartCap,
         EndEnergyCap = StartEnergyCap,
         EndChargeCap = StartChargeCap,
         NewCap = 0,
         RetCap = 0,
         NewEnergyCap = 0,
         RetEnergyCap = 0,
         NewChargeCap = 0,
         RetChargeCap = 0,
         year = 2020);
capacity <- rbind(temp_capacity, capacity);  
capacity_for_settlement <- capacity;
capacity_temp1 <- subset(capacity,Fuel == "ZCF") %>%
  mutate(Resource = paste(Resource,"_ZCF",sep = ""));
capacity <- rbind(capacity_temp1, subset(capacity,Fuel != "ZCF"));
capacity <- subset(capacity,select = -c(Fuel));


write.csv(capacity, paste0(outpath, sep, "capacity.csv"), row.names = FALSE)


#------------------------------------#
#     Combining RPS constraint data  #
#------------------------------------#
all_RPS_constr = allfiles[grep("/RPS.csv", allfiles)]

RPS_constr = read.csv(paste0(path,"/",all_RPS_constr[[1]]))
RPS_constr$case = substr(all_RPS_constr[[1]], find_year[2] + 1, find_case[2] - 1)
RPS_constr$year = substr(all_RPS_constr[[1]],1, 4)
if (length(all_RPS_constr)>1)
{
  for (f in 2:length(all_RPS_constr))
  {
    temp_RPS_constr = read.csv(paste0(path,"/",all_RPS_constr[[f]]))
    temp_RPS_constr$case = substr(all_RPS_constr[[f]], find_year[2] + 1, which(strsplit(all_RPS_constr[[f]], "")[[1]]=="/")[2] - 1)
    temp_RPS_constr$year = substr(all_RPS_constr[[f]],1, 4)
    RPS_constr <- bind_rows(RPS_constr, temp_RPS_constr)
  }
}

write.csv(RPS_constr, paste0(outpath, sep, "RPS_constraints.csv"), row.names = FALSE)

#--------------------------------#
#     Combining RPS Prices data  #
#--------------------------------#
all_RPS_CES = allfiles[grep("/RPS_prices.csv", allfiles)]
RPS_CES = read.csv(paste0(path,"/",all_RPS_CES[[1]]))
RPS_CES$case = substr(all_RPS_CES[[1]], find_year[2] + 1, find_case[2] - 1)
RPS_CES$year = substr(all_RPS_CES[[1]],1, 4)
RPS_CES$ID <- seq.int(nrow(RPS_CES))
if (length(all_RPS_CES)>1)
{
  for (f in 2:length(all_RPS_CES))
  {
    temp_RPS_CES = read.csv(paste0(path,"/",all_RPS_CES[[f]]))
    temp_RPS_CES$case = substr(all_RPS_CES[[f]], find_year[2] + 1, which(strsplit(all_RPS_CES[[f]], "")[[1]]=="/")[2] - 1)
    temp_RPS_CES$year = substr(all_RPS_CES[[f]],1, 4)
    temp_RPS_CES$ID <- seq.int(nrow(temp_RPS_CES))
    RPS_CES <- bind_rows(RPS_CES, temp_RPS_CES)
  }
}

write.csv(RPS_CES, paste0(outpath, sep, "RPS_CES.csv"), row.names = FALSE)


#----------------------------#
#     Combining Cost data    #
#----------------------------#
all_costs = allfiles[grep("/costs.csv", allfiles)]
costs = read.csv(paste0(path,"/",all_costs[[1]]))
costs$case = substr(all_costs[[1]], find_year[2] + 1, find_case[2] - 1)
costs$year = substr(all_costs[[1]],1, 4)
if (length(all_costs)>1)
{
  for (f in 2:length(all_costs))
  {
    temp_costs = read.csv(paste0(path,"/",all_costs[[f]]))
    temp_costs$case = substr(all_costs[[f]], find_year[2] + 1, which(strsplit(all_costs[[f]], "")[[1]]=="/")[2] - 1)
    temp_costs$year = substr(all_costs[[f]],1, 4)
    costs <- bind_rows(costs, temp_costs)
  }
}

write.csv(costs, paste0(outpath, sep, "costs.csv"), row.names = FALSE)

#-------------------------------------------------#
#     Combining Transmission Expansion results    #
#-------------------------------------------------#
all_trans = allfiles[grep("/network_expansion.csv", allfiles)]
trans = read.csv(paste0(path,"/",all_trans[[1]]))
trans$case = substr(all_trans[[1]], find_year[2] + 1, find_case[2] - 1)
trans$year = substr(all_trans[[1]],1, 4)
if (length(all_trans)>1)
{
  for (f in 2:length(all_trans))
  {
    temp_trans = read.csv(paste0(path,"/",all_trans[[f]]))
    temp_trans$case = substr(all_trans[[f]], find_year[2] + 1, which(strsplit(all_trans[[f]], "")[[1]]=="/")[2] - 1)
    temp_trans$year = substr(all_trans[[f]],1, 4)
    trans <- bind_rows(trans, temp_trans)
  }
}

write.csv(trans, paste0(outpath, sep, "trans.csv"), row.names = FALSE)

#------------------------------------#
#     Combining Powerflow results    #
#------------------------------------#
all_flows = allfiles[grep("/flow.csv", allfiles)]

flows = read.csv(paste0(path,"/",all_flows[[1]]))
flows$case = substr(all_flows[[1]], find_year[2] + 1, find_case[2] - 1)
flows$year = substr(all_flows[[1]],1, 4)
if (length(all_flows)>1)
{
  for (f in 2:length(all_flows))
  {
    temp_flows = read.csv(paste0(path,"/",all_flows[[f]]))
    temp_flows$case = substr(all_flows[[f]], find_year[2] + 1, which(strsplit(all_flows[[f]], "")[[1]]=="/")[2] - 1)
    temp_flows$year = substr(all_flows[[f]],1, 4)
    flows <- bind_rows(flows, temp_flows)
  }
}

write.csv(flows, paste0(outpath, sep, "flows.csv"), row.names = FALSE)



#--------------------------------#
#     Combining Power results    #
#--------------------------------#
all_power = allfiles[grep("/power.csv", allfiles)]; 
all_generators_data <- allfiles[grep("/Generators_data.csv", allfiles)];
power = t(read.csv(paste0(path,"/",all_power[[1]]),header = F)[1:3,]);
powercolnames <- power[1,];
power <- power[-c(1,length(power[,1])),];
colnames(power) <- powercolnames;
power <- as.data.frame(power);
t_generators <- read.csv(paste0(path,"/",all_generators_data[[1]]));
power <- cbind(power,t_generators$Fuel);
colnames(power)[dim(power)[2]] <- "Fuel";
power$case = substr(all_power[[1]], find_year[2] + 1, find_case[2] - 1)
power$year = substr(all_power[[1]],1, 4)

if (length(all_power)>1)
{
  for (f in 2:length(all_power)){
    temp_power = t(read.csv(paste0(path,"/",all_power[[f]]),header = F)[1:3,]);
    powercolnames <- temp_power[1,];
    temp_power <- temp_power[-c(1,length(temp_power[,1])),];
    colnames(temp_power) <- powercolnames;
    temp_power <- as.data.frame(temp_power);
    t_generators <- read.csv(paste0(path,"/",all_generators_data[[f]]));
    temp_power <- cbind(temp_power,t_generators$Fuel);
    colnames(temp_power)[dim(temp_power)[2]] <- "Fuel";
    temp_power$case = substr(all_power[[f]], find_year[2] + 1, which(strsplit(all_power[[f]], "")[[1]]=="/")[2] - 1)
    temp_power$year = substr(all_power[[f]],1, 4)
    power <- bind_rows(power, temp_power)
  }
}
power$Sum <- as.numeric(power$Sum);
power <- aggregate(Sum~case+year+Zone+Resource+Fuel,power,sum);

for (i in 1:length(power[,1]))
{
  power$region[i] <- zone_mapping$region[as.numeric(power$Zone[i])];
}
power_for_settlement <- power;

power_temp1 <- subset(power,Fuel == "ZCF") %>%
  mutate(Resource = paste(Resource,"_ZCF",sep = ""));
power <- rbind(power_temp1, subset(power,Fuel != "ZCF"));
power <- subset(power,select = -c(Fuel));

write.csv(power, paste0(outpath, sep, "power.csv"), row.names = FALSE);
#--------------------------------#
#     Combining charge results   #
#--------------------------------#

all_charge = allfiles[grep("/charge.csv", allfiles)]
charge = read.csv(paste0(path,"/",all_charge[[1]]))
charge$case = substr(all_charge[[1]], find_year[2] + 1, find_case[2] - 1)
charge$year = substr(all_charge[[1]],1, 4)

if (length(all_charge)>1)
{
  for (f in 2:length(all_charge))
  {
    temp_charge = read.csv(paste0(path,"/",all_charge[[f]]))
    temp_charge$case = substr(all_charge[[f]], find_year[2] + 1, which(strsplit(all_charge[[f]], "")[[1]]=="/")[2] - 1)
    temp_charge$year = substr(all_charge[[f]],1, 4)
    charge <- bind_rows(charge, temp_charge)
  }
}
write.csv(charge, paste0(outpath, sep, "charge.csv"), row.names = FALSE)

#-------------------------------------------#
#     Combining non-served energy results   #
#-------------------------------------------#
all_nse = allfiles[grep("/nse.csv", allfiles)]
nse = read.csv(paste0(path,"/",all_nse[[1]]))
nse$case = substr(all_nse[[1]], find_year[2] + 1, find_case[2] - 1)
nse$year = substr(all_nse[[1]],1, 4)
if(length(all_nse)>1)
{
  for (f in 2:length(all_nse))
  {
    temp_nse = read.csv(paste0(path,"/",all_nse[[f]]))
    temp_nse$case = substr(all_nse[[f]], find_year[2] + 1, which(strsplit(all_nse[[f]], "")[[1]]=="/")[2] - 1)
    temp_nse$year = substr(all_nse[[f]],1, 4)
    nse <- bind_rows(nse, temp_nse)
  }
}
write.csv(nse, paste0(outpath, sep, "nse.csv"), row.names = FALSE)

#--------------------------------------#
#     Combining energy price results   #
#--------------------------------------#
all_prices = allfiles[grep("/prices_w.csv", allfiles)]

if(length(all_prices>0))
{
  prices = as.data.frame(read.csv(paste0(path,"/",all_prices[[1]])));
  colnames(prices) <- c("hour",zone_mapping$region);
  prices$case = substr(all_prices[[1]], find_year[2] + 1, find_case[2] - 1);
  prices$year = substr(all_prices[[1]],1, 4);
  time_weight = read.csv(paste0(path,"/",all_time_weight[[1]]));
  prices$weight = time_weight$Weight;
  prices <- melt(data = prices, id.var = c("hour","weight","case","year"),measure.vars = zone_mapping$region);
  if (length(all_prices)>1)
  {
    for (f in 2:length(all_prices))
    {
      temp_prices = as.data.frame(read.csv(paste0(path,"/",all_prices[[f]])));
      colnames(temp_prices) <- c("hour",zone_mapping$region);
      temp_prices$case = substr(all_prices[[f]], find_year[2] + 1, which(strsplit(all_prices[[f]], "")[[1]]=="/")[2] - 1)
      temp_prices$year = substr(all_prices[[f]],1, 4)
      time_weight = read.csv(paste0(path,"/",all_time_weight[[f]]))
      temp_prices$weight = time_weight$Weight
      temp_prices <- melt(data = temp_prices, id.var = c("hour","weight","case","year"),measure.vars = zone_mapping$region);
      prices <- bind_rows(prices, temp_prices)
      
    }
  }
  write.csv(prices, paste0(outpath, sep, "prices.csv"), row.names = FALSE)
}

#---------------------------------#
#     Combining CO2 results       #
#---------------------------------#
all_CO2 = allfiles[grep("/emissions.csv", allfiles)]
CO2 = read.csv(paste0(path,"/",all_CO2[[1]]))
CO2$case = substr(all_CO2[[1]], find_year[2] + 1, find_case[2] - 1)
CO2$year = substr(all_CO2[[1]],1, 4)
if (length(all_CO2)>1)
{
  for (f in 2:length(all_CO2))
  {
    temp_CO2 = read.csv(paste0(path,"/",all_CO2[[f]]))
    temp_CO2$case = substr(all_CO2[[f]], find_year[2] + 1, which(strsplit(all_CO2[[f]], "")[[1]]=="/")[2] - 1)
    temp_CO2$year = substr(all_CO2[[f]],1, 4)
    CO2 <- bind_rows(CO2, temp_CO2)
  }
  write.csv(CO2, paste0(outpath, sep, "CO2.csv"), row.names = FALSE)
}

#----------------------------------------------------#
#     Combining Capacity Reserve Market results      #
#----------------------------------------------------#

all_Capprices = allfiles[grep("/ReserveMargin_w.csv", allfiles)];
Res_Mar = read.csv(paste0(path,"/",all_Capprices[[1]]));
# Res_Mar <- t(Res_Mar);
# ConstraintName <- paste0("CapRes_",c(1:ncol(Res_Mar)));
# Res_Mar <- as.data.frame(cbind(paste0("t",c(1:nrow(Res_Mar)),sep=""),Res_Mar));
# colnames(Res_Mar) <- c("hour",ConstraintName);
colnames(Res_Mar)[1] <- "hour";
ConstraintName <- colnames(Res_Mar)[-1]
Res_Mar <- as.data.frame(Res_Mar);
time_weight = read.csv(paste0(path,"/",all_time_weight[[1]]))
Res_Mar$weight = time_weight$Weight;
Res_Mar$case = substr(all_Capprices[[1]], find_year[2] + 1, find_case[2] - 1);
Res_Mar$year = substr(all_Capprices[[1]],1, 4);
Res_Mar <- melt(data = Res_Mar, id.vars = c("hour","weight","case","year"), measure.vars=ConstraintName,);

if (length(all_Capprices)>1)
{
  for (f in 2:length(all_Capprices))
  {
    temp_Res_Mar = read.csv(paste0(path,"/",all_Capprices[[f]]));
    # temp_Res_Mar = t(temp_Res_Mar);
    # ConstraintName <- paste0("CapRes_",c(1:ncol(temp_Res_Mar)));
    # temp_Res_Mar <- as.data.frame(cbind(paste0("t",c(1:nrow(temp_Res_Mar)),sep=""),temp_Res_Mar));
    # colnames(temp_Res_Mar) <- c("hour",ConstraintName);
    colnames(temp_Res_Mar)[1] <- "hour";
    ConstraintName <- colnames(temp_Res_Mar)[-1]
    temp_Res_Mar <- as.data.frame(temp_Res_Mar);
    time_weight = read.csv(paste0(path,"/",all_time_weight[[f]]));
    temp_Res_Mar$weight = time_weight$Weight;
    temp_Res_Mar$case = substr(all_Capprices[[f]], find_year[2] + 1, which(strsplit(all_Capprices[[f]], "")[[1]]=="/")[2] - 1);
    temp_Res_Mar$year = substr(all_Capprices[[f]],1, 4);
    temp_Res_Mar <- melt(data = temp_Res_Mar, id.vars = c("hour","weight","case","year"), measure.vars=ConstraintName,);
    Res_Mar <- bind_rows(Res_Mar, temp_Res_Mar);    
  }
}
write.csv(Res_Mar, paste0(outpath, sep, "Res_Mar.csv"), row.names = FALSE);

#-------------------------------------------#
#     Combining Generator data results      #
#-------------------------------------------#

all_generators_data = allfiles[grep("/Generators_data.csv", allfiles)]
generators = read.csv(paste0(path,"/",all_generators_data[[1]]))
generators$case = substr(all_generators_data[[1]], find_year[2] + 1, find_case[2] - 1)
generators$year = substr(all_generators_data[[1]],1, 4)
if (length(all_generators_data)>1)
{
  for (f in 2:length(all_generators_data))
  {
    temp_generators = read.csv(paste0(path,"/",all_generators_data[[f]]))
    temp_generators$case = substr(all_generators_data[[f]], find_year[2] + 1, which(strsplit(all_generators_data[[f]], "")[[1]]=="/")[2] - 1)
    temp_generators$year = substr(all_generators_data[[f]],1, 4)
    generators <- bind_rows(generators, temp_generators) 
  }
}
temp_capacity <- capacity;
temp_capacity$Zone <-as.numeric(temp_capacity$Zone);
temp_capacity$Cluster <-as.numeric(temp_capacity$Cluster);
write.csv(generators, paste0(outpath, sep, "generators.csv"), row.names = FALSE)
MaxoutCheck <- merge(generators,temp_capacity,by.x = c("region","Resource","zone","cluster","case","year"),by.y = c("Region","Resource","Zone","Cluster","case","year"),all.x=T);
MaxoutCheck <- aggregate(cbind(EndCap,Max_Cap_MW)~region+Resource+case+year,data = MaxoutCheck,sum)
MaxoutCheck <- MaxoutCheck[(MaxoutCheck$Max_Cap_MW >0.1) & (abs(MaxoutCheck$Max_Cap_MW-MaxoutCheck$EndCap)<1),];
write.csv(MaxoutCheck,paste0(outpath, sep, "maxout.csv"), row.names = FALSE)
#---------------------------------------#
#     Combining Gen   Settlement data   #
#---------------------------------------#
Settlementfn = allfiles[grep("/NetRevenue.csv", allfiles)];
all_generators_data <- allfiles[grep("/Generators_data.csv", allfiles)];
Settlement = read.csv(paste0(path,"/",Settlementfn[[1]]),header=T);
t_generators <- read.csv(paste0(path,"/",all_generators_data[[1]]));
Settlement <- cbind(Settlement,t_generators$Fuel);
colnames(Settlement)[dim(Settlement)[2]] <- "Fuel";
Settlement <- aggregate(cbind(
  Var_OM_cost_out	,
  Var_OM_cost_in,
  Fixed_OM_cost_MW,
  Fixed_OM_cost_MWh,
  Fuel_cost,
  Charge_cost,
  Inv_cost_MW,	
  Inv_cost_MWh,
  StartCost,
  EmissionsCost,
  EnergyRevenue,
  SubsidyRevenue,
  ReserveMarginRevenue,
  RPSRevenue
)~region+Resource+Fuel,Settlement,sum)
Settlement$case = substr(Settlementfn[[1]], find_year[2] + 1, find_case[2] - 1)
Settlement$year = substr(Settlementfn[[1]],1, 4)
if (length(Settlementfn)>1)
{
  for(f in 2:length(Settlementfn))
  {
    temp_Settlement = read.csv(paste0(path,"/",Settlementfn[[f]]),header=T)
    t_generators <- read.csv(paste0(path,"/",all_generators_data[[f]]));
    temp_Settlement <- cbind(temp_Settlement,t_generators$Fuel);
    colnames(temp_Settlement)[dim(temp_Settlement)[2]] <- "Fuel";
    temp_Settlement <- aggregate(cbind(
      Var_OM_cost_out	,
      Var_OM_cost_in,
      Fixed_OM_cost_MW,
      Fixed_OM_cost_MWh,
      Fuel_cost,
      Charge_cost,
      Inv_cost_MW,	
      Inv_cost_MWh	,
      StartCost,
      EmissionsCost,
      EnergyRevenue,
      SubsidyRevenue,
      ReserveMarginRevenue,
      RPSRevenue
    )~region+Resource+Fuel,temp_Settlement,sum)
    temp_Settlement$case = substr(Settlementfn[[f]], find_year[2] + 1, which(strsplit(Settlementfn[[f]], "")[[1]]=="/")[2] - 1)
    temp_Settlement$year = substr(Settlementfn[[f]],1, 4)
    Settlement <- bind_rows(Settlement, temp_Settlement)
  }
}



Settlement <- Settlement %>% plyr::rename(c("region" = "Region"));

CapacityforRevenueCal <- aggregate(EndCap~Region+Resource+case+year+Fuel,capacity_for_settlement,sum);
PowerforLCOECal <- aggregate(Sum~region+Resource+case+year+Fuel,power_for_settlement,sum);
PowerforLCOECal <- PowerforLCOECal %>% plyr::rename(c("region" = "Region"));


Settlement <- merge(Settlement,CapacityforRevenueCal,by = c("Region","Resource","Fuel", "case","year"),all.x=T)
Settlement <- merge(Settlement,PowerforLCOECal,by = c("Region","Resource","Fuel", "case","year"),all.x=T)

Settlement$Region <- as.factor(Settlement$Region);
Settlement$year <- as.factor(Settlement$year);
Settlement$case <- as.factor(Settlement$case);
Settlement$Resource <- as.factor(Settlement$Resource);
Settlement$Fuel <- as.factor(Settlement$Fuel);
Settlement$Var_OM_cost_out = -1*Settlement$Var_OM_cost_out;
Settlement$Var_OM_cost_in = -1*Settlement$Var_OM_cost_in;
Settlement$Inv_cost_MW = -1*Settlement$Inv_cost_MW;
Settlement$Inv_cost_MWh = -1*Settlement$Inv_cost_MWh;
Settlement$Fixed_OM_cost_MWh = -1*Settlement$Fixed_OM_cost_MWh;
Settlement$Fuel_cost = -1*Settlement$Fuel_cost;
Settlement$Charge_cost = -1*Settlement$Charge_cost;
Settlement$EmissionsCost = -1*Settlement$EmissionsCost;
Settlement$StartCost = -1*Settlement$StartCost;
Settlement$Fixed_OM_cost_MW = -1*Settlement$Fixed_OM_cost_MW;
Settlement$VOM_n_Fuel <- Settlement$Var_OM_cost_out+Settlement$Var_OM_cost_in+Settlement$Fuel_cost+Settlement$StartCost;
Settlement$FOM <- Settlement$Fixed_OM_cost_MWh + Settlement$Fixed_OM_cost_MW;
Settlement$Inv_cost <- Settlement$Inv_cost_MW + Settlement$Inv_cost_MWh;
SunkCost <- subset(Settlement,select = c("Region","Resource","Fuel","case","year"));
SunkCost$SunkCost = 0;

for (i in 1:nrow(SunkCost))
{
  if (SunkCost$year[i] == "2040")
  {
    temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource,Fuel, case,year)) %>%
      filter(year == "2030") %>%
      filter(case == SunkCost$case[i]) %>%
      filter(Resource == SunkCost$Resource[i]) %>%
      filter(Region == SunkCost$Region[i]) %>%
      filter(Fuel == SunkCost$Fuel[i]);
    SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
  }
  else if ((SunkCost$year[i] == "2050") & (SunkCost$Resource[i] != "battery_mid"))
  {
    temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource, Fuel,case,year)) %>%
      filter((year == "2030") | (year == "2040")) %>%
      filter(case == SunkCost$case[i]) %>%
      filter(Resource == SunkCost$Resource[i]) %>%
      filter(Region == SunkCost$Region[i]) %>%
      filter(Fuel == SunkCost$Fuel[i]);
    SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
  }
  else if ((SunkCost$year[i] == "2050") & (SunkCost$Resource[i] == "battery_mid"))
  {
    temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource, Fuel, case,year)) %>%
      filter(year == "2040") %>%
      filter(case == SunkCost$case[i]) %>%
      filter(Resource == SunkCost$Resource[i]) %>%
      filter(Region == SunkCost$Region[i])%>%
      filter(Fuel == SunkCost$Fuel[i]);
    SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
  }
}

if (years[1] == '2030'){
  for (i in 1:nrow(SunkCost))
  {
    if (SunkCost$year[i] == "2040")
    {
      temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource,Fuel, case,year)) %>%
        filter(year == "2030") %>%
        filter(case == SunkCost$case[i]) %>%
        filter(Resource == SunkCost$Resource[i]) %>%
        filter(Region == SunkCost$Region[i]) %>%
        filter(Fuel == SunkCost$Fuel[i]);
      SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
    }
    else if ((SunkCost$year[i] == "2050") & (SunkCost$Resource[i] != "battery_mid"))
    {
      temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource, Fuel,case,year)) %>%
        filter((year == "2030") | (year == "2040")) %>%
        filter(case == SunkCost$case[i]) %>%
        filter(Resource == SunkCost$Resource[i]) %>%
        filter(Region == SunkCost$Region[i]) %>%
        filter(Fuel == SunkCost$Fuel[i]);
      SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
    }
    else if ((SunkCost$year[i] == "2050") & (SunkCost$Resource[i] == "battery_mid"))
    {
      temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource, Fuel, case,year)) %>%
        filter(year == "2040") %>%
        filter(case == SunkCost$case[i]) %>%
        filter(Resource == SunkCost$Resource[i]) %>%
        filter(Region == SunkCost$Region[i])%>%
        filter(Fuel == SunkCost$Fuel[i]);
      SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
    }
  }
} else if (years[1] == '2022'){
  for (i in 1:nrow(SunkCost))
  {
    if (SunkCost$year[i] == "2025")
    {
      temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource,Fuel, case,year)) %>%
        filter(year == "2022") %>%
        filter(case == SunkCost$case[i]) %>%
        filter(Resource == SunkCost$Resource[i]) %>%
        filter(Region == SunkCost$Region[i]) %>%
        filter(Fuel == SunkCost$Fuel[i]);
      SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
    }
    else if ((SunkCost$year[i] == "2030"))
    {
      temp_inv <- subset(Settlement, select=c(Inv_cost,Region,Resource, Fuel,case,year)) %>%
        filter((year == "2022") | (year == "2025")) %>%
        filter(case == SunkCost$case[i]) %>%
        filter(Resource == SunkCost$Resource[i]) %>%
        filter(Region == SunkCost$Region[i]) %>%
        filter(Fuel == SunkCost$Fuel[i]);
      SunkCost$SunkCost[i] <- sum(temp_inv$Inv_cost);
    }
  }
}

Settlement <- merge(Settlement,SunkCost,by = c("Region","Resource","Fuel","case","year"),all.x=T);
Settlement_temp1 <- subset(Settlement,Fuel == "ZCF") %>%
  mutate(Resource = paste(Resource,"_ZCF",sep = ""));
Settlement <- rbind(Settlement_temp1, subset(Settlement,Fuel != "ZCF"));
Settlement <- subset(Settlement,select = -c(Fuel));

Settlement_Short <- subset(Settlement, select = c("Region","Resource","case","year",
                                                  "EnergyRevenue","SubsidyRevenue",
                                                  "ReserveMarginRevenue",
                                                  "RPSRevenue","VOM_n_Fuel","FOM","Inv_cost",
                                                  "SunkCost","Charge_cost","EmissionsCost","EndCap","Sum"));
Regional_Settlement <- aggregate(cbind(
  EnergyRevenue	,
  SubsidyRevenue,
  ReserveMarginRevenue,
  RPSRevenue,
  VOM_n_Fuel,
  FOM,
  Inv_cost,	
  SunkCost,
  Charge_cost,
  EmissionsCost
)~Region+case+year,Settlement,sum)

write.csv(Regional_Settlement, paste0(outpath, sep, "Regional_Settlement.csv"), row.names = FALSE)
write.csv(Settlement, paste0(outpath, sep, "Settlement.csv"), row.names = FALSE)
write.csv(Settlement_Short, paste0(outpath, sep, "Settlement_short.csv"), row.names = FALSE)

#---------------------------------------------------#
#     Combining Capacity Reserve Value results      #
#---------------------------------------------------#

all_CapValue = allfiles[grep("/CapacityValue.csv", allfiles)];
CapValue = read.csv(paste0(path,"/",all_CapValue[[1]]));
time_weight = read.csv(paste0(path,"/",all_time_weight[[1]]))
CapValue <- as.data.frame(CapValue);
CapValue <- CapValue[,c(1,2,length(CapValue),3:(length(CapValue)-1))];
CapValue_hour <- CapValue;
CapValue_hour[,c(4:length(CapValue_hour))] <- ceiling(CapValue_hour[,c(4:length(CapValue_hour))]);
CapValue_hour$sum <- as.matrix(CapValue_hour[,c(4:length(CapValue_hour))]) %*% time_weight[,2]
CapValue$value <- as.matrix(CapValue[,c(4:length(CapValue))]) %*% time_weight[,2]
CapValue$value <- CapValue$value/CapValue_hour$sum;
CapValue<-CapValue[,c(1:3,length(CapValue))]
CapValue_hour <-CapValue_hour[,c(1:3,length(CapValue_hour))]
CapValue$case = substr(all_CapValue[[1]], find_year[2] + 1, find_case[2] - 1);
CapValue$year = substr(all_CapValue[[1]],1, 4);
CapValue_hour$case = substr(all_CapValue[[1]], find_year[2] + 1, find_case[2] - 1);
CapValue_hour$year = substr(all_CapValue[[1]],1, 4);

t_power = t(read.csv(paste0(path,"/",all_power[[1]]),header = F)[1:3,]);
powercolnames <- t_power[1,];
t_power <- t_power[-c(1,length(t_power[,1])),];
colnames(t_power) <- powercolnames;
t_power <- as.data.frame(t_power);

CapValue <-cbind(CapValue,rep(t_power$Sum,2))



if (length(all_CapValue)>1)
{
  for (f in 2:length(all_CapValue))
  {
    temp_CapValue = read.csv(paste0(path,"/",all_CapValue[[f]]));
    time_weight = read.csv(paste0(path,"/",all_time_weight[[f]]))
    temp_CapValue <- as.data.frame(temp_CapValue);
    temp_CapValue <- temp_CapValue[,c(1,2,length(temp_CapValue),3:(length(temp_CapValue)-1))];
    temp_CapValue_hour <- temp_CapValue;
    temp_CapValue_hour[,c(4:length(temp_CapValue_hour))] <- ceiling(temp_CapValue_hour[,c(4:length(temp_CapValue_hour))]);
    temp_CapValue_hour$sum <- as.matrix(temp_CapValue_hour[,c(4:length(temp_CapValue_hour))]) %*% time_weight[,2]
    temp_CapValue$value <- as.matrix(temp_CapValue[,c(4:length(temp_CapValue))]) %*% time_weight[,2]
    temp_CapValue$value <- temp_CapValue$value/temp_CapValue_hour$sum;
    temp_CapValue<-temp_CapValue[,c(1:3,length(temp_CapValue))]
    temp_CapValue_hour <-temp_CapValue_hour[,c(1:3,length(temp_CapValue_hour))]
    temp_CapValue_hour$case = substr(all_CapValue[[f]], find_year[2] + 1, which(strsplit(all_CapValue[[f]], "")[[1]]=="/")[2] - 1);
    temp_CapValue_hour$year = substr(all_CapValue[[f]],1, 4);
    temp_CapValue$case = substr(all_CapValue[[f]], find_year[2] + 1, which(strsplit(all_CapValue[[f]], "")[[1]]=="/")[2] - 1);
    temp_CapValue$year = substr(all_CapValue[[f]],1, 4);
    
    t_power = t(read.csv(paste0(path,"/",all_power[[f]]),header = F)[1:3,]);
    powercolnames <- t_power[1,];
    t_power <- t_power[-c(1,length(t_power[,1])),];
    colnames(t_power) <- powercolnames;
    t_power <- as.data.frame(t_power);
    temp_CapValue <-cbind(temp_CapValue,rep(t_power$Sum,2))
    CapValue <- bind_rows(CapValue, temp_CapValue);  
    CapValue_hour <- bind_rows(CapValue_hour, temp_CapValue_hour); 
  }
}
write.csv(CapValue, paste0(outpath, sep, "CapValue.csv"), row.names = FALSE);
write.csv(CapValue_hour, paste0(outpath, sep, "CapValue_hour.csv"), row.names = FALSE);

