library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(imputeTS)
library(zoo)
library(tsibble)
library(slider)
library(car)
#library(astsa)


#load data
dat<-read_csv(file=here("data","combined_top9_WLIS_clean_dat.csv"))

# monthly plots -----------------------------------------------------------

ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month/1000)) +
  geom_point()+
  geom_line(col='darkgrey')+
  ylab('monthly total N load by outfall (1000 kg N/mo)')+
  ggtitle('Monthly totals top 9 facilities Western LIS only')+
  xlab('Date')+
  facet_wrap(~facility, scales = "free")


ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month/1000)) +
  geom_point()+
  #geom_line(col='darkgrey')+
  ylab('monthly total N load by outfall (1000 kg N/mo)')+
  ggtitle('Monthly totals top 9 facilities Western LIS only')+
  xlab('Date')+
  facet_wrap(~facility)


# annual sum to compare to LIS tracker--------------------------------------------------------------

duplicates(dat)
dat<-distinct(dat)

annual_sum<-dat %>%
  group_by(permit_outfall, facility,year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T),
            count_n=n()) %>%
  arrange(desc(count_n))
annual_sum

ggplot(annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (1000 kg N/yr)')+
  ggtitle('Annual totals top 9 facilities Western LIS only')+
  facet_wrap(~facility)

annual<-read_csv(here('data','annual_N_loads_QAQC.csv'))
annual<-annual %>%
  pivot_longer(cols=c(as.character(2015), as.character(2016), as.character(2017),
                      as.character(2018),as.character(2019),as.character(2020)), 
               names_to='year')

annual$year<-format(annual$year, format='%Y')

annual$key<-paste0(annual$NPDES_ID,annual$year)
annual_sum$key<-paste0(substr(annual_sum$permit_outfall,1,9),annual_sum$year)

join<-left_join(annual_sum, annual, by='key')

join<-join %>% 
  mutate(kg_N_TN_yr_LIS=value*365/2.205) %>%
  mutate(difference=(kg_N_TN_yr-kg_N_TN_yr_LIS))


ggplot(join,aes(x=kg_N_TN_yr/1000,y=kg_N_TN_yr_LIS/1000,col=facility.x, shape=facility.x))+
  geom_point()+
  geom_abline(slope=1,line_type='dashed', col='red')+
  theme_minimal()+
  scale_shape_manual(values=c(1:9))+
  xlim(0,6000)+
  xlab('ECHO/CTDEEP data')+
  ylab('LIS tracker')


outliers<-join %>%filter(abs(difference)>5000) %>%
  arrange(desc(difference)) %>%
  select(facility.x,year.x,kg_N_TN_yr,kg_N_TN_yr_LIS,difference)
outliers




# impute time series missing data -----------------------------------------

#first removing duplicates
dat$kg_N_TN_per_month<-round(dat$kg_N_TN_per_month,2)
dat<- dat %>%
  select(permit, facility, key,permit_outfall, kg_N_TN_per_month, month_year,
         outfall=Outfall,state, LATITUDE83,LONGITUDE83,huc8,name,season) %>%
  distinct()

#drop day from month year format
#dat_ts$month_year<-format(as.Date(dat_ts$month_year), "%Y-%m")
dat$month_year<-yearmonth(dat$month_year)
strptime(dat$month_year, "%Y %b")

#create tsibble object
dat_ts<-as_tsibble(dat, key = facility, index=month_year)
dat_ts

#inspecting missingingness
has_gaps(dat_ts)
scan_gaps(dat_ts)
ts_gaps<-count_gaps(dat_ts)

#visualizing time gaps
ggplot(ts_gaps, aes(x = facility, colour = facility)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme_minimal()+
  ggtitle('Data gaps in time')+
  theme(legend.position = "bottom")


#impute 36-month running average 
full_ts<- dat_ts %>%
  group_by_key() %>% 
  mutate(kg_N_TN_per_month_complete=kg_N_TN_per_month)%>%
  fill_gaps(kg_N_TN_per_month_complete=mean(kg_N_TN_per_month, na.rm=T), 
            .full = TRUE)

#quantify how many data were imputed
full_ts_n<-as_tibble(full_ts)%>% 
            group_by(facility) %>% 
             select(facility,kg_N_TN_per_month_complete) %>%
            summarise(full_ts_n=n()) 
dat_n<-dat %>%
  group_by(facility) %>%
  select(facility,kg_N_TN_per_month) %>%
  summarise(dat_ts_n=n()) 
n_join<-left_join(full_ts_n,dat_n)
n_join<-n_join %>%
        mutate(n_imputed=full_ts_n-dat_ts_n,
               pct_imputed=n_imputed/full_ts_n*100)
n_join
#wanted to customize this more, but not working yet
# fill_gaps(kg_N_TN_per_month,.full = FALSE)#make NAs explicit by filling in missing months as "NA"
# mutate(imputed=is.na(kg_N_TN_per_month))%>% #noting which data are NA that we will impute
# group_by_key() %>% 
# mutate(rolling_mean_36 =
#          slide_dbl(kg_N_TN_per_month,
#                    ~ mean(., na.rm = TRUE),  
#                    size=36)
#        )%>%
# mutate(rolling_mean_3 =
#          slide_dbl(kg_N_TN_per_month,
#                    ~ mean(., na.rm = TRUE),  
#                    size=3)
# )%>%
# mutate(kg_N_TN_per_month_complete=
#          case_when(
#    is.na(kg_N_TN_per_month) ~ mean(kg_N_TN_per_month,na.rm=T), #impute rolling mean from 36 month window when needed
#   !is.na(kg_N_TN_per_month) ~ kg_N_TN_per_month))

#broken but want to use in future
# mutate(kg_N_TN_per_month_complete=
#          case_when(
#            #(is.na(kg_N_TN_per_month) & is.na(rolling_mean_3)) ~ rolling_mean_36, #impute rolling mean from 36 month window when needed
#            #(is.na(kg_N_TN_per_month) & !is.na(rolling_mean_3))~ rolling_mean_3, #impute mean from only 3 months when possible
#            !is.na(kg_N_TN_per_month) ~kg_N_TN_per_month))

summary(full_ts$kg_N_TN_per_month)
summary(full_ts$kg_N_TN_per_month_complete) #this imputes the mean across all time



ggplot(full_ts, aes(x=month_year, y=kg_N_TN_per_month_complete/1000)) +
  #geom_point()+
  geom_line()+
  ylab('monthly total N load by outfall (1000 kg N/mo)')+
  ggtitle('Monthly totals top 9 facilities Western LIS only; data gaps imputed')+
  xlab('Date')+
  theme_minimal()+
  facet_wrap(~facility)




# #sample of data summary by time index
# dat_ts%>% 
#   group_by_key() %>%
#   index_by(year = year(month_year)) %>% 
#   summarise(
#     Nload_high = max(kg_N_TN_per_month, na.rm = TRUE),
#     Nload_low = min(kg_N_TN_per_month, na.rm = TRUE)
#   )

dat$month_year<-as.Date.character(dat$month_year)

write_csv(dat,
          file = here("data", 'combined_top9_WLIS_full_ts.csv'))


# seasons analysis --------------------------------------------------------
dat_nonzero<-filter(dat_ts,kg_N_TN_per_month>0 &
                      !is.na(permit_outfall) &
                      !is.na(season))

hist(dat_nonzero$kg_N_TN_per_month_complete)

meq1=lmer(log(kg_N_TN_per_month) ~ season+ (1|permit_outfall), data=dat_nonzero, 
          na.action=NULL)

summary(meq1) 
#r.squaredGLMM(meq1)

#adding the intercept (fall) to all model fits
coef <- data.frame(data = rep(fixef(meq1)[('(Intercept)')],4) +
                     c(0,fixef(meq1)['seasonspring'], 
                       fixef(meq1)['seasonsummer'], 
                       fixef(meq1)['seasonwinter'])
)

se <- sqrt(diag(vcov(meq1))) #standard errors from meq1


meq1_outputs<- cbind(coef, se)
colnames(meq1_outputs) <- c('coef', 'SE')
meq1_outputs$season<-c('fall','spring','summer','winter')
meq1_outputs


season_plot<-ggplot() +
  geom_jitter(data=dat_nonzero, aes(x=as.factor(season), y=log(kg_N_TN_per_month)),width=.05,alpha=.4) +
  geom_violin(data=dat_nonzero, aes(x=as.factor(season), y=log(kg_N_TN_per_month)),fill='lightgrey', color='lightgrey',alpha=.4)+
  geom_pointrange(data=meq1_outputs,
                  aes(x=season,y=coef,ymin = coef-SE, ymax =  coef+SE), 
                  col='red',
                  size=1) +
  theme_minimal()+
  xlab('Monthly loads by season')+
  ylab('ln (monthly total N load by outfall (kg N/mo))')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  annotate(geom='text',x=c(1.1,2.1,3.1,4.1), y=c(13.8,14.8,14.25,14.8), label=c('a','b','ab','b'),
           fontface =2)
season_plot


# 
# #fig 4
# seasons_plot<-ggplot() +
#   geom_violin(dat_ts, aes(x=season, y=kg_N_TN_per_month_complete)) +
#   geom_jitter(dat_ts, aes(x=season, y=kg_N_TN_per_month_complete, 
#               width=.05, alpha=.3)) +
#   ylab('monthly total N load by outfall (kg N/mo)')+
#   
#   geom_pointrange(data = meq2_outputs,aes(reorder(names, coef), coef, 
#                                           ymin = coef-SE, ymax =  coef+SE), col='red') +
#   theme(axis.text.x = element_text(angle=30, hjust=1)) +
#   scale_x_discrete('' ,labels=x_axis) +
#   geom_segment(aes(x=0,xend=0,y=-2,yend=3.5), colour="black") +
#   geom_hline(yintercept=-2, color = "black") +
#   theme_default(axis_text_size = 13) +
#   geom_vline(xintercept=c(1.5,3.5, 5.5, 7.5), linetype='longdash') 
# seasons_plot



hist(dat$kg_N_TN_per_month)

summary(lm(log(dat_nonzero$kg_N_TN_per_month)~as.factor((dat_nonzero$season))))

lmer1<-lmer(log(kg_N_TN_per_month)~
              as.factor(season) + 
              (1|permit_outfall), data=dat_nonzero)
summary(lmer1)

# load vs. hypoxia --------------------------------------------------------


hyp<-read_csv(file=here("data",'CTDEEP_hypoxia.csv'),
              col_types = c('cnn'))

hyp$Start_Date_UTC<-ymd_hms(hyp$Start_Date_UTC)
hyp$Start_Date_UTC<-as_date(hyp$Start_Date_UTC)

hyp$month_year<-substr(hyp$Start_Date_UTC,1,7)
hyp$month_year<-ym(hyp$month_year)

min(hyp$month_year, na.rm=T)
max(hyp$month_year, na.rm=T)

hyp_yr<-hyp %>%
  group_by(year=year(month_year)) %>%
  summarise(max_Area_under_3_mgL =max(Area_under_3_mgL, na.rm=T),
            mean_Area_under_3_mgL =mean(Area_under_3_mgL, na.rm=T),
            max_Area_under_5_mgL =max(Area_under_5_mgL, na.rm=T),
            mean_Area_under_5_mgL =mean(Area_under_5_mgL, na.rm=T))
hyp_yr

##correlation between spring + summer total load and hypoxia area
spring_summer<-as_tibble(full_ts) %>% 
  mutate(year=year(month_year)) %>%
  select(season, year, kg_N_TN_per_month_complete) %>%
  filter(season=='spring' | season=='summer') %>%
  group_by(year) %>%
  summarise(spring_summer_total=sum(kg_N_TN_per_month_complete))


hyp_dat<-left_join(hyp_yr,spring_summer)
hyp_dat<-filter(hyp_dat,max_Area_under_5_mgL>0) #remove zeroes for plotting

ggplot(hyp_dat,aes(x=year,y=max_Area_under_5_mgL)) +
  geom_point()+
  geom_smooth(method = "lm")+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('date')+
  theme_minimal()

ggplot(hyp_dat,aes(x=spring_summer_total/1000,y=max_Area_under_5_mgL)) +
  geom_point()+
  ylab(expression(paste0('area under 5 mg/L dissolved oxygen (',km^2,')'))) +
  xlab('total Spring and summer N load (1,000 kg N/yr)')+
  theme_minimal()+
  ggtitle('Annual totals top 9 facilities Western LIS only')

##test between winter + spring total load and hypoxia

spring_winter<-as_tibble(full_ts) %>% 
  mutate(year=year(month_year)) %>%
  select(season, year, kg_N_TN_per_month_complete) %>%
  filter(season=='spring' | season=='winter') %>%
  group_by(year) %>%
  summarise(spring_winter_total=sum(kg_N_TN_per_month_complete))


hyp_dat<-left_join(hyp_yr,spring_winter)
hyp_dat<-filter(hyp_dat,max_Area_under_5_mgL>0) #remove zeroes for plotting

ggplot(hyp_dat,aes(x=year,y=max_Area_under_5_mgL)) +
  geom_point()+
  geom_smooth(method = "lm")+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('date')+
  theme_minimal()

hyp_dat_no_na<-hyp_dat %>%
  select(year,max_Area_under_5_mgL,spring_winter_total) %>%
  mutate(spring_winter_total_.001=spring_winter_total/1000) %>%
  drop_na()
summary(lm(max_Area_under_5_mgL~spring_winter_total_.001, dat=hyp_dat_no_na))

ggplot(hyp_dat,aes(x=spring_winter_total/1000,y=max_Area_under_5_mgL)) +
  geom_point()+
  geom_smooth(method='lm')+
  ylab(expression('area under 5 mg/L dissolved oxygen '~(km^2))) +
  xlab('total winter and spring N load (1,000 kg N/yr)')+
  theme_minimal()+
  ggtitle('Annual totals top 9 facilities Western LIS only')+
  annotate('text', x=10500,y=2400,label='R2= 0.08', fontface=2)



##

hyp<-read_csv(file=here("data",'JOD_hyp_area_volume.csv'))
hyp$year<-year(ym(hyp$date))


ggplot(hyp,aes(x=year,y=area_4.8mg_l_km2)) +
  geom_point()+
  #geom_smooth(method = "lm")+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('date')+
  theme_minimal()


# test for seasonality ----------------------------------------------------
# 
# #see https://online.stat.psu.edu/stat510/lesson/4/4.2
# #and https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2030&context=jmasm
# #https://www.r-bloggers.com/2021/04/timeseries-analysis-in-r/
# #http://fable.tidyverts.org/
# 
# 
# #these commands below are designed for time series of n=1, but I need to figure out how to
# #analyze for the many different time series in our data for each permit/outfall
# 
# 
# Chicopee_monthly_totals<-dat_joined %>%
#   filter(name=='Chicopee River') %>%
#   group_by(month_year) %>%
#   summarise(kg_N_TN_month=sum(kg_N_TN_per_month, na.rm = T))
# 
# plot(Chicopee_monthly_totals)
# 
# Chicopee_monthly_totals_ts<-ts(Chicopee_monthly_totals)
# Chicopee_monthly_totals_ts
# 
# diff12=diff(Chicopee_monthly_totals_ts, 12)
# plot(diff12)
# acf2(diff12)
# diff1and12 = diff(diff12, 1) 
# 
# diff4 = diff(Chicopee_monthly_totals_ts, 4)
# diff1and4 = diff(diff4,1)
# acf2(diff1and4,24)
