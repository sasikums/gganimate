us_zip = read.csv('C:/Users/shreedharsasikumar/Downloads/us_zip_code_weekly_indexes (9).csv')

library(dplyr)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(reshape2)
library(tidyr)
library(zoo)

setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog')

us_zip_select =us_zip %>%
  select(us_zipcode5,
         zcta,
         classification,
         us_countyfipscode5,
         us_countyname,
         us_statename,
         us_stateshort,
         standardized_date,
         population,
         zip_population,
         population_density,
         county_population,
         state_population,
         covid_cases_cumulative,
         covid_cases_cumulative_per_capita,
         covid_deaths_cumulative,
         covid_deaths_cumulative_per_capita,
         svi,
         pop_over65_percentile,
         zcta_acs_race_percent_white,
         zcta_acs_race_percent_africanamerican,
         zcta_acs_race_percent_hispanic,
         zcta_acs_race_percent_asian,
         zcta_acs_race_percent_nativeamericanalaskan,
         zcta_acs_gini_index,
         zcta_acs_household_income_median,
         zcta_acs_household_income_percentile20,
         zcta_acs_household_income_percentile80,
         zcta_acs_disability_percentage_anydisability,
         zcta_acs_households_noearners_percentage,
         zcta_acs_households_recieving_public_assistance_percentage,
         zcta_acs_education_population_noschool_percentage,
         zcta_acs_education_population_advanceddegree_percentage,
         zcta_acs_language_population_percent_spainish,
         zcta_acs_language_population_percent_englishonly,
         county_cvac,county_cvac_category)

write.csv(us_zip_select,
          'C:/Users/shreedharsasikumar/Downloads/us_zip_select.csv',row.names = FALSE)

us_zip_select=read.csv('C:/Users/shreedharsasikumar/Downloads/us_zip_select.csv',stringsAsFactors = FALSE)

us_zcta_select=filter(us_zip_select,us_zipcode5==zcta)
us_zcta_race=us_zcta_select%>%
  select(zcta,
         population,
         zcta_acs_household_income_median,
         zcta_acs_race_percent_white,
         zcta_acs_race_percent_africanamerican,
         zcta_acs_race_percent_hispanic,
         zcta_acs_race_percent_asian,
         zcta_acs_race_percent_nativeamericanalaskan)%>%
  mutate(PopScore=zcta_acs_household_income_median*population,
         bin_white=100*round(percent_rank(zcta_acs_race_percent_white),2),
         bin_africanamerican=100*round(percent_rank(zcta_acs_race_percent_africanamerican),2),
         bin_hispanic=100*round(percent_rank(zcta_acs_race_percent_hispanic),2),
         bin_asian=100*round(percent_rank(zcta_acs_race_percent_asian),2),
         bin_nativeamerican=100*round(percent_rank(zcta_acs_race_percent_nativeamericanalaskan),2)
         )

emp_frame=data.frame(bin=1:100)
race_vars=c("zcta_acs_race_percent_white","zcta_acs_race_percent_africanamerican","zcta_acs_race_percent_hispanic",
            "zcta_acs_race_percent_asian","zcta_acs_race_percent_nativeamericanalaskan")
race_bins=c("bin_white","bin_africanamerican","bin_hispanic","bin_asian","bin_nativeamerican")
race_labels=c("White","AfricanAmerican","Hispanic","Asian","NativeAmerican")
base_frame <- data.frame()
for(i in (1:5)){
  dummy=us_zcta_race
  
  names(dummy)[names(dummy) == race_bins[i]] <- "bin"
  
  race_sum = dummy %>%
    group_by(bin)%>%
    mutate(PopScore=population*zcta_acs_household_income_median)%>%
    summarise(PopScore=sum(PopScore,na.rm = TRUE),
              avg_race=mean(zcta_acs_race_percent_africanamerican,na.rm=TRUE),
              n_zips=n_distinct(zcta,na.rm = TRUE),
              population=sum(population,na.rm = TRUE)
    )%>%
    mutate(Avg=PopScore/population)%>%
    ungroup()
  
  clean_race=emp_frame%>%
    left_join(race_sum)%>%
    fill(Avg,.direction="down")%>%
    mutate(Label=race_labels[i])
  
  base_frame = bind_rows(base_frame,clean_race)
  
}



anim= ggplot(base_frame, aes(x=bin, y=Avg)) +
  geom_line(aes(linetype=Label, color=Label)) +
  theme_ipsum() +
  ylab("Median Household Income $") +
  transition_reveal(bin)

animate(anim, duration = 20, fps = 20, width = 800, height = 500, renderer = gifski_renderer())

# Save at gif:
anim_save("test_anim.gif")
base_frame=data.frame()
for(i in (1:5)){
  dummy=us_zcta_race
  names(dummy)[names(dummy) == race_vars[i]] <- "relv_race"
  collector=data.frame()
  for(j in (0:100)){
    dum_sub=dummy%>%
      filter(relv_race>=j/100)%>%
      summarise(SumScore=sum(PopScore,na.rm = TRUE),
                SumPop=sum(population,na.rm = TRUE),
                CountZCTA=n_distinct(zcta))%>%
      mutate(bin=j,Avg=SumScore/SumPop)%>%
      mutate(Avg=ifelse(SumPop<1000,NA,Avg))
    #print(dum_sub)
    #print(nrow(dum_sub))
      
    collector=bind_rows(collector,dum_sub)
    rm(dum_sub)
  }
  collector= mutate(collector, 
                    Race=race_labels[i], 
                    Label=paste0(race_labels[i],":$",round(Avg/1000,0),"K (",bin,"%)"))
  base_frame =bind_rows(base_frame,collector)
}
base_frame = base_frame %>%
  arrange(bin, -Avg)%>%
  group_by(bin)%>%
  mutate(Rank=1:n())%>%
  ungroup()%>%
  mutate(ypos=max(Avg,na.rm=TRUE)*(1.1-(0.1*Rank)))

    mutate(base_frame,bin=bin/100)
plot = ggplot(base_frame, aes(x=bin, y=Avg)) +
  geom_line(aes(linetype=Race, color=Race)) +
  theme_ipsum() +
  ylab("Median Household Income $") +
  xlab("Minimum % of Race") +
  #scale_x_continuous(labels = scales::percent)+
  scale_color_viridis_d() +
  geom_label(label=base_frame$Label,x=25,y=base_frame$ypos)+
  theme(axis.title.y = element_text(size =16), 
         axis.title.x = element_text(size=  16, hjust = .4),
         axis.text.x = element_text(color = "black", size = 14),
         axis.text.y = element_text(size = 12, color = "black"),
         plot.title = element_text(hjust = 0.5,
                                   size = 18,
                                   face = "bold"),
         legend.position = "top")
  

anim = plot + 
  transition_reveal(bin)+
  labs(title=paste0("Median Household Income by Min % of Population in ZCTA:","{frame_along}","%"))
animate(anim, duration = 10, fps = 5, width = 800, height = 500, renderer = gifski_renderer())

# Save at gif:
anim_save("test_anim.gif")
