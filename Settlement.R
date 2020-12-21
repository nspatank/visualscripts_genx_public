



###################################################################################
dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/Settlement/", showWarnings = FALSE)

Settlement_csv <-read.csv("Settlement_short.csv",header=T)
Settlement_csv_Join <- left_join(Settlement_csv, resource_mapping )

Settlement_csv_Join <-  plyr::rename(Settlement_csv_Join,c("Region" = "Agg_region"))

Settlement_csv_Join <- filter(Settlement_csv_Join,EndCap>20)
Settlement_csv_Join$case <- factor(Settlement_csv_Join$case,levers <- cases);
for (j in 1:length(years)) {
  
  AllRevenue_working <- subset(Settlement_csv_Join, select = -c(Resource)) %>%
    filter(year == years[j])
  PJM_NJLand_AllRevenue <- filter(AllRevenue_working, Agg_region == "PJM_NJLand")
  PJM_NJLand_AllRevenue <- aggregate(
    . ~ Agg_region+ Fuel+case+year, PJM_NJLand_AllRevenue, sum)
  PJM_NJCoast_AllRevenue <- filter(AllRevenue_working, Agg_region == "PJM_NJCoast")
  PJM_NJCoast_AllRevenue <- aggregate(. ~Agg_region+ Fuel+case+year, PJM_NJCoast_AllRevenue, sum)
  NJ_AllRevenue <- filter(AllRevenue_working, grepl("NJ", Agg_region));
  NJ_AllRevenue$Agg_region = "Total NJ";
  NJ_AllRevenue <- aggregate(. ~Agg_region+ Fuel+case+year, NJ_AllRevenue, sum)
  all_NJ_AllRevenue <- rbind(PJM_NJLand_AllRevenue, PJM_NJCoast_AllRevenue, NJ_AllRevenue)
  all_NJ_AllRevenue$case = 
    factor(all_NJ_AllRevenue$case, levels = cases)
  all_NJ_AllRevenue_norm <- subset(all_NJ_AllRevenue,select=-c(EndCap,Sum));
  all_NJ_AllRevenue_norm[,c(5:(ncol(all_NJ_AllRevenue_norm)))]<-all_NJ_AllRevenue_norm[,c(5:(ncol(all_NJ_AllRevenue_norm)))]/all_NJ_AllRevenue$EndCap;
  
  all_NJ_AllRevenue_Profit <- cbind(all_NJ_AllRevenue[,1:4],rowSums(all_NJ_AllRevenue[,c(5:(ncol(all_NJ_AllRevenue_norm)))]));
  colnames(all_NJ_AllRevenue_Profit) = c("Agg_region","Fuel","case","year","Profit");
  all_NJ_AllRevenue_Profit_norm <- all_NJ_AllRevenue_Profit;
  all_NJ_AllRevenue_Profit_norm[,5] <- all_NJ_AllRevenue_Profit[,5]/all_NJ_AllRevenue$EndCap;
  
  all_NJ_AllRevenue <- melt(all_NJ_AllRevenue,id=c("Agg_region","Fuel","case","year")) %>% filter(variable != "EndCap") %>% filter(variable != "Sum")
  all_NJ_AllRevenue_norm <- melt(all_NJ_AllRevenue_norm,id=c("Agg_region","Fuel","case","year"))
  all_NJ_AllRevenue_Profit_norm <- melt(all_NJ_AllRevenue_Profit_norm,id=c("Agg_region","Fuel","case","year"))
  
  # ggplot(all_NJ_AllRevenue, aes(x=Fuel, y = value/1e6, fill = variable)) +
  #   geom_bar(position="stack", stat="identity") + 
  #   facet_grid(case~Agg_region) + 
  #   theme_bw()+
  #   scale_fill_brewer(name = "Revenue Type", palette = "Paired")+
  #   xlab("")+
  #   ylab("Million$/year")+
  #   ggsave(paste0("Graphics/Settlement/", years[j], "_Settlement.png"), width=30, height=10, dpi=300)
  # ggplot(all_NJ_AllRevenue_norm, aes(x=Fuel, y = value/1e3, fill = variable)) +
  #   geom_bar(position="stack", stat="identity") +
  #   facet_grid(case~Agg_region) + 
  #   theme_bw()+
  #   scale_fill_brewer(name = "Revenue Type", palette = "Paired")+
  #   xlab("")+
  #   ylab("Thousand$/MW-year") +
  #   ggsave(paste0("Graphics/Settlement/", years[j], "_Settlement_Norm.png"), width=30, height=10, dpi=300)
    ggplot() +
      geom_bar(aes(x = Fuel, y = value/1e3, fill = variable), data = all_NJ_AllRevenue_norm, position="stack", stat="identity") +
      geom_point(aes(x= Fuel, y=value/1e3),data = all_NJ_AllRevenue_Profit_norm,stat="identity")+
      facet_grid(case~Agg_region) + 
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      scale_fill_brewer(name = "Revenue Type", palette = "Paired")+
      xlab("")+
      ylab("Thousand$/MW-year") +
      ggsave(paste0("Graphics/Settlement/", years[j], "_Settlement_Norm_withProfit.png"), width=10, height=10, dpi=300)
}

for (j in 1:length(years))
{
  
  AllRevenue_working <- subset(Settlement_csv_Join, select = -c(Resource)) %>%
    filter(year == years[j])
  PJM_West_AllRevenue <- filter(AllRevenue_working, Agg_region == "PJM_WEST")
  PJM_West_AllRevenue <- aggregate(. ~ Agg_region+ Fuel+case+year, PJM_West_AllRevenue, sum);
  PJM_West_AllRevenue_norm <- subset(PJM_West_AllRevenue,select=-c(EndCap,Sum));
  PJM_West_AllRevenue_norm[,c(5:(ncol(PJM_West_AllRevenue_norm)))]<-PJM_West_AllRevenue_norm[,c(5:(ncol(PJM_West_AllRevenue_norm)))]/PJM_West_AllRevenue$EndCap
  
  PJM_West_AllRevenue_Profit <- cbind(PJM_West_AllRevenue[,1:4],rowSums(PJM_West_AllRevenue[,c(5:(ncol(PJM_West_AllRevenue)))]));
  colnames(PJM_West_AllRevenue_Profit) = c("Agg_region","Fuel","case","year","Profit");
  PJM_West_AllRevenue_Profit_norm <- PJM_West_AllRevenue_Profit;
  PJM_West_AllRevenue_Profit_norm[,5] <- PJM_West_AllRevenue_Profit[,5]/PJM_West_AllRevenue$EndCap;
  
  
  
  PJM_West_AllRevenue <- melt(PJM_West_AllRevenue,id=c("Agg_region","Fuel","case","year")) %>% filter(variable != "EndCap") %>% filter(variable != "Sum")
  PJM_West_AllRevenue_norm <- melt(PJM_West_AllRevenue_norm,id=c("Agg_region","Fuel","case","year"))
  PJM_West_AllRevenue_Profit_norm <- melt(PJM_West_AllRevenue_Profit_norm,id=c("Agg_region","Fuel","case","year"))
  
  # ggplot(PJM_West_AllRevenue_noProfit, aes(x=Fuel, y = value/1e6, fill = variable)) +
  #   geom_bar(position="stack", stat="identity") + 
  #   facet_grid(~case) + 
  #   theme_bw()+
  #   scale_fill_brewer(name = "Revenue Type", palette = "Paired")+
  #   xlab("")+
  #   ylab("Million$/year")+
  #   ggsave(paste0("Graphics/Settlement/", years[j], "_PJM_West_Settlement.png"), width=30, height=10, dpi=300)
  # ggplot(PJM_West_AllRevenue_norm_noProfit, aes(x=Fuel, y = value/1e3, fill = variable)) +
  ggplot() +
    geom_bar(aes(x = Fuel, y = value/1e3, fill = variable), data = PJM_West_AllRevenue_norm, position="stack", stat="identity") +
    geom_point(aes(x= Fuel, y=value/1e3),data = PJM_West_AllRevenue_Profit_norm,stat="identity")+
    facet_grid(case~Agg_region) + 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    scale_fill_brewer(name = "Revenue Type", palette = "Paired")+
    xlab("")+
    ylab("Thousand$/MW-year") +
    ggsave(paste0("Graphics/Settlement/", years[j], "PJM_West_Settlement_Norm_withProfit.png"), width=10, height=10, dpi=300)
}
