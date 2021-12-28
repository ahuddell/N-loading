#load data
dat<-read_csv(file=here("data","clean_PCS_ECHO_dat.csv"))

# monthly plots -----------------------------------------------------------

# ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month/1000)) +
#   geom_point()+
#   geom_line(col='darkgrey')+
#   ylab('monthly total N load by outfall (1000 kg N/mo)')+
#   xlab('Date')+
#   facet_wrap(~facility, scales = "free")


# annual sum to compare to LIS tracker--------------------------------------------------------------

#checking for duplicate keys
dat %>%
  group_by(key)%>%
  tally() %>%
  filter(n>1)
#there are none

annual_sum<-dat %>%
  group_by(permit_outfall, state,facility,year=year(month_year)) %>%
  summarise(count_n=n(),
            kg_N_TN_d=sum(kg_N_TN_per_month, na.rm = T)/(count_n*30)) %>%
  arrange((count_n))
annual_sum

# annual_sum %>% 
#   filter(state=='CT')%>%
# ggplot(aes(x=year,y=kg_N_TN_yr/1000)) +
#   geom_point()+
#   geom_smooth(method = "loess")+
#   ylab('Annual total N load by outfall (1000 kg N/yr)')+
#   facet_wrap(~facility, scales="free_y")
# 
# annual_sum %>% 
#   filter(state!='CT')%>%
#   ggplot(aes(x=year,y=kg_N_TN_yr/1000)) +
#   geom_point()+
#   geom_smooth(method = "loess")+
#   ylab('Annual total N load by outfall (1000 kg N/yr)')+
#   facet_wrap(~facility, scales="free_y")

check<-read_csv(here('data','annual_N_loads_QAQC.csv'))
check<-check %>%
  pivot_longer(cols=c(as.character(1994),as.character(1995),as.character(1996),
                      as.character(1997),as.character(1998),as.character(1999),
                      as.character(2000),as.character(2001),as.character(2002),
                      as.character(2003),as.character(2004),as.character(2005),
                      as.character(2006),as.character(2007),as.character(2008),
                      as.character(2009),as.character(2010),as.character(2011),
                      as.character(2012),as.character(2013),as.character(2014),
                      as.character(2015), as.character(2016), as.character(2017),
                      as.character(2018),as.character(2019),as.character(2020)), 
               names_to='year')

check$year<-format(check$year, format='%Y')

check$key<-paste0(check$NPDES_ID,check$year)
annual_sum$key<-paste0(substr(annual_sum$permit_outfall,1,9),annual_sum$year)

join<-left_join(annual_sum, check, by='key')

join<-join %>% 
  mutate(kg_N_TN_d_LIS=value/2.205) %>%
  mutate(difference=(kg_N_TN_d-kg_N_TN_d_LIS))

#check ECHO/PCS data through time
ggplot(join,aes(x=year.x,y=kg_N_TN_d))+
  geom_point()+
  theme_minimal()+
  ylab('ECHO/CTDEEP data') +
  facet_wrap(~facility.x)

#check LIS tracker data through time
ggplot(join,aes(x=year.x,y=kg_N_TN_d_LIS))+
  geom_point()+
  theme_minimal()+
  ylab('ECHO/CTDEEP data') +
  facet_wrap(~facility.x)

ggplot(join,aes(x=kg_N_TN_d,y=kg_N_TN_d_LIS
                #,col=facility.x, shape=facility.x
))+
  geom_point()+
  geom_abline(slope=1,linetype='dashed', col='red')+
  theme_minimal()+
  # scale_shape_manual(values=c(1:133))+
  #  ylim(0,2000)+
  xlab('ECHO/CTDEEP data')+
  ylab('LIS tracker')+
  annotate(geom='text',x=2500, y=20000, label="R2=0.996",
           fontface =2)


summary(lm(join$kg_N_TN_d_LIS~join$kg_N_TN_d))