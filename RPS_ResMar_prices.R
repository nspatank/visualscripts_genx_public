
dir.create("Graphics", showWarnings = FALSE)
dir.create("Graphics/prices", showWarnings = FALSE)

all_inputs = read.csv(file="sample_inputs_pjm.csv", header=TRUE,na.strings="")
zone_mapping <- na.omit(select(all_inputs, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)

#plot for RPS prices
RPS_prices <- as.character(na.omit(all_inputs$RPS_price))

if (RPS_prices %in% c("Yes","Y","YES"))
{
RPS_CES_csv <- read.csv(file="RPS_CES.csv", header=TRUE, sep=",")

RPS_CES_csv$case <- factor(RPS_CES_csv$case, levels = ordered_set)
RPS_CES_csv$year <-as.character(RPS_CES_csv$year)
RPS_CES_csv$Policy[RPS_CES_csv$ID==1] <- "PJM_RPS"
RPS_CES_csv$Policy[RPS_CES_csv$ID==2] <- "PJM_CES"
RPS_CES_csv$Policy[RPS_CES_csv$ID==3] <- "MISO_RPS"
RPS_CES_csv$Policy[RPS_CES_csv$ID==4] <- "NY_RPS"
RPS_CES_csv$Policy[RPS_CES_csv$ID==5] <- "NY_CES"
RPS_CES_csv$Policy[RPS_CES_csv$ID==6] <- "SC_RPS"
RPS_CES_csv$Policy[RPS_CES_csv$ID==7] <- "100%_RPS"

ggplot(RPS_CES_csv , aes(x=year, y=RPS_Price, fill = case))+
    geom_bar(stat="identity",width = 0.6) + 
    theme_bw()+ 
    facet_grid(Policy~case) +
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
    labs(x="cases", y="$/MWh") +
  ggsave("Graphics/prices/RPS_price.png", width=6, height=6, dpi=300)
}

# plots for energy prices
Energy_prices <- as.character(na.omit(all_inputs$Energy_price))

if (Energy_prices %in% c("Yes","Y","YES"))
{
  
  prices_csv <- read.csv(file="prices.csv", header=TRUE, sep=",")
  # prices_csv <- melt(prices_csv_, id.vars = c("case", "year", "Zone","weight"))
  cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""]) 
  years <- as.character(na.omit(all_inputs$list_years))
  zones <- unique(prices_csv$variable);
  Interested_Regions <- as.character(na.omit(all_inputs$Price_Interested_Region))
  prices_csv$case = factor(prices_csv$case, levels = cases)
  
for (j in 1:length(years)) 
  {
  prices_csv_working <- prices_csv %>%
    filter(year == years[j])
  prices_csv_primary <- prices_csv_working %>%
    group_by(case, variable) %>%
    arrange(desc(value), .by_group = TRUE)
  price_plot <- data.frame()
  for (i in 1: length(unique(cases)))
  {
    for (k in 1:length(unique(zones)))
    {
      temp_prices <- subset(prices_csv_primary,case==cases[i] & variable == zones[k])
      temp_prices$count <- cumsum(temp_prices$weight)
      temp_prices$proc <- temp_prices$count/sum(temp_prices$weight)
      price_plot <-rbind(price_plot,temp_prices)
    }
  }
  price_plot <- price_plot %>%
    ungroup() %>%
    filter(variable %in% Interested_Regions)
  
  ggplot(price_plot , aes(x=proc, y=value, color = variable, group=variable))+
    geom_line(size=0.5) + 
    geom_hline(yintercept=0, color = "grey") +
    theme_bw()+ 
    coord_cartesian(ylim=c(-30,100))+
    facet_grid(~case)+
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(x="percent of hours", y="$ per megawatt-hour")+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
    ggsave(paste0("Graphics/prices/", years[j], "Eng_prices_by_case.png"), width=7, height=3, dpi=300)
  
  price_plot_2 <- subset(price_plot, price_plot$proc < .05)
  ggplot(price_plot_2 , aes(x=proc, y=value, color = variable, group=variable))+
    geom_line(size=0.5) + 
    geom_hline(yintercept=0, color = "grey") +
    theme_bw()+ 
    facet_grid(~case)+
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
    labs(x="percent of hours", y="$ per megawatt-hour")+
    scale_x_continuous(labels = scales::percent_format(accuracy =0.1))+
    ggsave(paste0("Graphics/prices/", years[j], "Eng_prices_by_case_zoom.png"), width=7, height=3, dpi=300)
  }
}

#Reserve margin prices
 Res_Mar_prices <- as.character(na.omit(all_inputs$Reserve_Margin_price))

if (Res_Mar_prices %in% c("Yes","Y","YES")){
  Res_Mar_csv <- read.csv(file="Res_Mar.csv", header=TRUE, sep=",")
  # Res_Mar_csv <- melt(Res_Mar_csv_, id.vars = c("case", "year", "constraint"))
  Res_Mar_csv$case = factor(Res_Mar_csv$case, levels = cases)
  cases <- na.omit(all_inputs$Interested_Set[all_inputs$Interested_Set!= ""]); 
  constraints <- unique(Res_Mar_csv$variable);
  years <- as.character(na.omit(all_inputs$list_years));
  
  for (j in 1:length(years)) {
    Res_Mar_csv_working <- Res_Mar_csv %>%
      filter(year == years[j])
    
    Res_Mar_csv_primary <- Res_Mar_csv_working %>%
      group_by(case, variable) %>%
      arrange(desc(value), .by_group = TRUE) 
    Res_Mar_price_plot <- data.frame();
    for (i in 1: length(unique(cases)))
    {
      for (k in 1:length(unique(constraints)))
      {
        temp_Res_prices <- subset(Res_Mar_csv_primary,case==cases[i] & variable == constraints[k])
        temp_Res_prices$count <- cumsum(temp_Res_prices$weight)
        temp_Res_prices$proc <- temp_Res_prices$count/sum(temp_Res_prices$weight)
        Res_Mar_price_plot <-rbind(Res_Mar_price_plot,temp_Res_prices)
      }
    }
    Res_Mar_price_plot <- Res_Mar_price_plot %>% ungroup()
    
    ggplot(Res_Mar_price_plot , aes(x=proc, y=value, color = case, group=case))+
      geom_line(size=0.5) + 
      theme_bw()+ 
      #ylim(0,200)+
      geom_hline(yintercept=0, color = "grey") +
      facet_grid(~variable)+
      theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      labs(x="percent of hours", y="$ per megawatt-hour")+
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
      ggsave(paste0("Graphics/prices/", years[j], "Res_Mar_prices_by_case.png"), width=7, height=3, dpi=300)
    
    Res_Mar_price_plot_2 <- subset(Res_Mar_price_plot, Res_Mar_price_plot$proc < (50/8760))
    ggplot(Res_Mar_price_plot_2 , aes(x=proc, y=value, color = case, group=case))+
      geom_line(size=0.5) + 
      geom_hline(yintercept=0, color = "grey") +
      theme_bw()+ 
      facet_grid(~variable)+
      theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
      labs(x="percent of hours", y="$ per megawatt-hour")+
      scale_x_continuous(labels = scales::percent_format(accuracy =0.1))+
      ggsave(paste0("Graphics/prices/", years[j], "Res_Mar_prices_by_case_zoom.png"), width=7, height=3, dpi=300)
  }
}
  
  
  
  
  
  
