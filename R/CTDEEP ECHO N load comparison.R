


#Format and add in the CT data

```{r}

CT<-read_csv(file=here("data","CT_NX_data.csv"))

CT<-distinct(CT) #remove a few duplicate rows

#formatting CT data to join with rest of ECHO data
CT_summary<-CT %>% 
  mutate(date=mdy(date)) %>%
  mutate(month_year=format(as.Date(date), "%Y-%m")) %>%
  mutate(days = day(date)) %>%
  mutate(Outfall=rep(1)) %>%
  mutate(kg_N_TN_per_period=N_load_lbs_day*days/2.205) %>%
  group_by(permit,Outfall,month_year) %>%
  summarise(kg_N_TN_per_month=sum(kg_N_TN_per_period)) %>%
  mutate(permit_outfall=paste0(permit,"_",Outfall)) %>%
  mutate(key = paste0(permit, "_", Outfall, "_", month_year)) 

names(CT)
permit_facility<-CT %>%
  group_by(permit,facility) %>%
  summarize(permit = first(permit),
            facility = first(facility))

CT_summary<-left_join(CT_summary, permit_facility)


N_load<- N_load %>%  
  mutate(month_year=format(as.Date(date), "%Y-%m")) %>%
  select(-date)

N_load$Outfall<-as.character(N_load$Outfall)
CT_summary$Outfall<-as.character(CT_summary$Outfall)



#join locations based on permit number and outfall number
N_load_CT_join <- rbind(N_load, CT_summary)

dim(N_load_CT_join) #43681     
dim(N_load) #25,745
dim(CT_summary) #17,936
length(unique(as.character(N_load_CT_join$permit_outfall))) #264 outfalls with locations
length(unique(as.character(N_load_CT_join$key))) #38,159 keys


```
