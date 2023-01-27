library(dplyr)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(ggthemes)
library(reshape2)
library(tidyr)
library(zoo)
library(stringr)

setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog/JCP/')

acs_select=read.csv('ACS 2020 Select Variables.csv',stringsAsFactors = FALSE)%>%
  mutate(zcta=str_pad(zcta,5,pad="0"),
         dollar_value_1K=Population_2020*(Per_Capita_inome/1000),
         retail_labor_dollar_value_1K=(Median_Income_Retail/1000)*Population_2020*(occupation_industry_retail/occupation_industry_total)
         )
  

swatch_scores=read.csv('Monthly Vibrancy by Zip Jan 2023.csv',stringsAsFactors = FALSE)%>%
  arrange(zipcode,zcta,standardized_date)%>%
  group_by(zipcode,zcta)%>%
  mutate(zcta=str_pad(zcta,5,pad="0"),
         YoYGrowth_BuildPermits_value=(bldg_permits_total_value/lag(bldg_permits_total_value,12)-1),
         YoYGrowth_SBA_Lender=(sba_loan_count/lag(sba_loan_count,12)-1),
         YoYGrowth_RetailSpend=(population_weight_sales/lag(population_weight_sales,12)-1),
         YoYGrowth_Median_ResidentialSalesPPSF=(median_ppsf_all_residential/lag(median_ppsf_all_residential,12)-1),
         M6Growth_BuildPermits_value=(bldg_permits_total_value/lag(bldg_permits_total_value,6)-1),
         M6Growth_SBA_Lender=(sba_loan_count/lag(sba_loan_count,6)-1),
         M6Growth_RetailSpend=(population_weight_sales/lag(population_weight_sales,6)-1),
         M6Growth_Median_ResidentialSalesPPSF=(median_ppsf_all_residential/lag(median_ppsf_all_residential,6)-1)
         )%>%
  ungroup()



retail_locations_zctas=read.csv('JCP and retail stores mapped to surrounding zctas.csv',stringsAsFactors = FALSE)%>%
  mutate(location_zcta=str_pad(location_zcta,5,pad="0"),
         Match_ZCTA=str_pad(Match_ZCTA,5,pad="0")
         )

relv_distance=30

main_brand='JCPenney'
relv_brands=c('JCPenney',"Kohl's" ,"Marshalls","Dillard's" ,"Bloomingdale's" ,"Macy's",
              "T.J. Maxx","Burlington Stores",'Bluemercury')
relv_brands_long=c('JCPenney','Bluemercury',"Kohl's" ,"Marshalls","Dillard's" ,"Bloomingdale's" ,
              "Macy's","T.J. Maxx","Burlington Stores","Target")

retail_top = retail_locations_zctas%>%
  filter(list_name %in% relv_brands,DistanceMiles<=relv_distance)%>%
  select(ali,list_name,location_zcta,zcta=Match_ZCTA)%>%
  left_join(select(acs_select,zcta,dollar_value_1K))%>%
  group_by(ali,list_name)%>%
  summarise(total_value=sum(dollar_value_1K,na.rm = TRUE))%>%
  ungroup()%>%
  group_by(list_name)%>%
  arrange(list_name,-total_value)%>%
  mutate(Rank=1:n())%>%
  ungroup()%>%
  filter(Rank<=100)
 

swatch_select=swatch_scores%>%
  select(zcta, state,standardized_date,county_unemployed_rate,unemployed_rate,median_ppsf_all_residential,
         avg_sale_to_list_all_residential,total_observed_sales,population_weight_sales,starts_with("YoY_"),
         starts_with("M6_"),
         econ_temp)%>%
  filter(standardized_date>=as.Date("2019-06-01"))%>%
  pivot_longer(cols = -c('zcta','state','standardized_date'))%>%
  mutate(Value=ifelse(value==Inf,NA,value))%>%
  left_join(select(acs_select,zcta,Population_2020,Per_Capita_inome,Median_Income_Retail,occupation_industry_retail,
                   occupation_industry_total,dollar_value_1K,retail_labor_dollar_value_1K))%>%
  mutate(PopScore=value*Population_2020,
         WeightPop=ifelse(is.na(value),0,as.numeric(Population_2020)),
         RetailScore=value*Population_2020*(occupation_industry_retail/occupation_industry_total),
         WeightRetail=ifelse(is.na(value),0,as.numeric(Population_2020*(occupation_industry_retail/occupation_industry_total)))
         )#%>%
#  filter(name %in% c('unemployed_rate','avg_sale_to_list_all_residential','total_observed_sales',
 #                    'population_weight_sales'))



# Total Portfolio ---------------------------------------------------------


swatch_retail = retail_locations_zctas%>%
  filter(list_name %in% relv_brands,DistanceMiles<=relv_distance)%>%
  select(ali,list_name,location_zcta,zcta=Match_ZCTA)%>%
  #group_by(list_name)%>%
  #summarise(Store_count=n_distinct(ali),Zip_count=n_distinct(zcta))%>%
  #ungroup()%>%
  left_join(filter(swatch_select,name=="econ_temp"))%>%
  filter(!is.na(standardized_date))%>%
  group_by(list_name,name,standardized_date)%>%
  summarise(PopScore=sum(PopScore,na.rm = TRUE),
            SumValue=sum(value,na.rm = TRUE),
            WeightPop=sum(WeightPop,na.rm = TRUE),
            RetailScore=sum(RetailScore,na.rm = TRUE),
            WeightRetail=sum(WeightRetail,na.rm = TRUE),
            Population_2020=sum(Population_2020,na.rm = TRUE),
            Store_count=n_distinct(ali)
              )%>%
  ungroup()%>%
  mutate(Avg=PopScore/WeightPop)%>%
  arrange(standardized_date, -Avg)%>%
  group_by(standardized_date)%>%
  mutate(Rank=1:n())%>%
  ungroup()%>%
  mutate(ypos=ifelse(Rank<5,60,57),
         Label=paste0(Rank,".",list_name," MVI:",round(Avg,2)),
         Month=as.Date(standardized_date))%>%
  rename(Brand=list_name)%>%
  mutate(xpos=ifelse(Rank<5,
                     as.Date(min(Month,na.rm = TRUE)+180*Rank),
                     as.Date(min(Month,na.rm = TRUE)+180*(Rank-5))
                     ))%>%
  mutate(xpos=as.Date(xpos))

plot = ggplot(swatch_retail, aes(x=Month, y=Avg)) +
  geom_line(aes(linetype=Brand, color=Brand),linewidth=1.5) +
  theme_ipsum() +
  ylab("Market Vibrancy Index") +
  xlab("Month-Year") +
  #scale_x_continuous(labels = scales::percent)+
 # geom_label(data=swatch_retail,aes(label=Label,x=xpos,y=ypos))+
  geom_label(data=swatch_retail,aes(label=Brand,x=Month+30,y=Avg,group=Brand))+
  theme_fivethirtyeight()+
  theme(axis.title.y = element_text(size =16), 
        axis.title.x = element_text(size=  16, hjust = .4),
        axis.text.x = element_text(color = "black", size = 0),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"),
        legend.position = "top")

plot
anim = plot + 
  transition_reveal(Month)+
  #view_follow(fixed_y = TRUE)  +
  labs(title=paste0("Market Vibrancy by Brand Across All Markets ",
                    "{frame_along}"),
       subtitle=paste0("Calculated as weighted average vibrancy across all individual markets"),
       caption="Source: BLS, Redfin, ACS 2020, AggData, Second Measure SBA etc.- Uses All Zips within 30 miles of store for each market")
animate(anim, duration = 30, fps = 5, width = 800, height = 500, renderer = gifski_renderer())
# Save at gif:
anim_save(paste0("Anim_","econ_temp",".gif"))

animate(anim, duration = 30, fps = 5, width = 800, height = 500, renderer = av_renderer())
# Save at mp4
anim_save(paste0("Anim_","econ_temp",".mp4"))


# Bar charts portfolio ----------------------------------------------------

p <- ggplot(swatch_retail) + #, aes(x = Rank, group = Country, country = as.factor(Code)
  geom_col(aes(x = Rank, y = Avg, color = Brand, fill=Brand), width = 0.8) + # Columns
  coord_flip(clip = "off", expand = FALSE) + # Flip
  ylab("Market Vibrancy Index") +
  xlab("")+
  theme_fivethirtyeight()+
  geom_text(aes(x = Rank, y = 45 , label = Brand), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Avg + 2, label = as.character(round(Avg,2))), hjust = 0, color = "black") + # Values  
  # geom_flag(aes(x = Rank, y = -300,  country = Code), size = 10) + # Flags
  scale_y_continuous(labels = scales::comma) + # Format y-axis values
  scale_x_reverse() + # Highest values on top
  #transition_states(Month, transition_length = 4, state_length = 1) + # Animate
  theme(axis.text.y = element_text(size =0), 
          axis.title.x = element_text(size=  0, hjust = .4),
          axis.text.x = element_text(color = "black", size = 16),
          plot.title = element_text(hjust = 0.5,
                                    size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = 14,
                                       face = "bold"),
          legend.position = "top")
p
anim_bar = p + 
  transition_states(Month, transition_length = 6, state_length = 3) + # Animate
  #view_follow(fixed_y = TRUE)  +
  labs(title=paste0("Market Vibrancy by Brand Across All Markets ",
                    "{closest_state}"),
       subtitle=paste0("Calculated as weighted average vibrancy across all individual markets"),
       caption="Source: BLS, Redfin, ACS 2020, AggData, Second Measure, SBA etc. - Uses All Zips within 30 miles of store for each market")
animate(anim_bar, duration = 20, fps = 5, width = 800, height = 500, renderer = gifski_renderer())
# Save at gif:
anim_save(paste0("BarAnim_","econ_temp",".gif"))

animate(anim_bar, duration = 30, fps = 5, width = 800, height = 500, renderer = av_renderer())
# Save at mp4
anim_save(paste0("BarAnim_","econ_temp",".mp4"))


# By store bubbles --------------------------------------------------------



swatch_alli = retail_locations_zctas %>%
  filter(list_name %in% relv_brands,DistanceMiles<=relv_distance)%>%
  select(ali,list_name,location_zcta,zcta=Match_ZCTA)%>%
  inner_join(distinct(retail_top,ali))%>%
  #group_by(list_name)%>%
  #summarise(Store_count=n_distinct(ali),Zip_count=n_distinct(zcta))%>%
  #ungroup()%>%
  left_join(swatch_select)%>%
  filter(!is.na(standardized_date))%>%
  group_by(ali,list_name,name,standardized_date)%>%
  summarise(PopScore=sum(PopScore,na.rm = TRUE),
            WeightPop=sum(WeightPop,na.rm = TRUE),
            RetailScore=sum(RetailScore,na.rm = TRUE),
            WeightRetail=sum(WeightRetail,na.rm = TRUE),
            Population_2020=sum(Population_2020,na.rm = TRUE),
            SumValue=sum(value,na.rm = TRUE),
            Store_count=n_distinct(ali)
  )%>%
  ungroup()%>%
  mutate(Avg=PopScore/WeightPop,
         Month=as.Date(standardized_date))%>%
  mutate(Avg=ifelse(name=='population_weight_sales',SumValue,Avg))

swatch_chart = swatch_alli %>%
  pivot_wider(id_cols = c('ali','list_name','Month'),names_from=name,
                          values_from = Avg)%>%
  left_join(retail_top)%>%
  rename(Brand=list_name)


p <- ggplot(
  swatch_chart, 
  aes(y = avg_sale_to_list_all_residential, x=unemployed_rate, size = population_weight_sales, colour = Brand)
) +
  geom_point( alpha = 0.7) +
  theme_fivethirtyeight()+
  #scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  labs(y = "Residential Sale to List Ratio (Sale $/List $)", x = "Imputed Zip Unemployment Rate (%)")+
  theme(axis.title.y = element_text(size =16), 
        axis.title.x = element_text(size=  16, hjust = .4),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                  size = 12),
        
        legend.position = "top")+
  guides(size = "none",fill=guide_legend(title="Brand"))    


p
single_chart= p + 
  geom_text( aes(x = max(swatch_chart$unemployed_rate,na.rm = TRUE),
                y = min(swatch_chart$avg_sale_to_list_all_residential,na.rm = TRUE), 
                label = as.factor(Month)) , hjust=-2, vjust = -0.2, alpha = 0.4,  col = "gray", size = 12)+ 
  transition_time(Month)+
  labs(title = paste0("Unemployment vs. Residential Sale to List Ratio around Stores by Brand & Month: ",
                      " {frame_time}"),
       subtitle="Each circle represents a store sized by Observed Retail Sales $ in those markets",
       caption="Source: BLS, Redfin, ACS 2020, AggData, Second Measure - Uses all ZCTAs with 30 miles of store;Top 50 Markets by Income & Pop")

animate(single_chart, duration = 30, fps = 5, width = 800, height = 500, renderer = gifski_renderer())
anim_save(paste0("Bubble Single",".gif"))
animate(single_chart, duration = 30, fps = 5, width = 800, height = 500, renderer = av_renderer())
anim_save(paste0("Bubble Single",".mp4"))

facet_chart = p + 
  facet_wrap(~Brand)+
  transition_time(Month) +
  shadow_mark(alpha = 0.5, size = 0.5) +
  labs(title = paste0("Unemployment vs. Residential Sale to List Ratio by Month: ",
                      " {frame_time}"),
       subtitle="Each circle represents a store sized by Observed Retail Sales $ in those markets",
       caption="Source: BLS, Redfin, ACS 2020, AggData, Second Measure - Uses all ZCTAs with 30 miles of store;Top 50 Markets by Income & Pop")

animate(facet_chart, duration = 30, fps = 5, width = 800, height = 500, renderer = gifski_renderer())
anim_save(paste0("Bubble Facet",".gif"))
animate(facet_chart, duration = 30, fps = 5, width = 800, height = 500, renderer = av_renderer())
anim_save(paste0("Bubble Facet",".mp4"))

