#load libraries
library(tidyverse)
library(lubridate)
library(here)
library(patchwork)

#read in data
dat<-read_csv(file=unzip(zipfile=here('data','complete_time_series_with_missing_data_imputed.zip')))
names(dat)


#group facilities by state
dat_state<-dat %>%
  group_by(state, date) %>%
  summarise(kgN_mo=sum(kg_N_TN_per_month_complete,na.rm=T))

#reorder states 
dat_state$state <- factor(dat_state$state, levels=c('NY', 'MA', 'CT', 'NH'))
factor(dat_state$state)

fig2<-ggplot(data =dat_state,
       aes(x = date, y = kgN_mo/1000, col = state,
           fill=state)) +
  ylab(expression(paste(
    'N load (Mg N',~mo^-1,')')))+
  xlab('Date')+
  theme_minimal() +
  geom_line(alpha = 0.7, size=1) +
  theme(legend.position = 'bottom')+
  scale_color_viridis(name='State', discrete=TRUE) +
  scale_fill_viridis(name='State', discrete=TRUE)

ggsave('fig2.jpeg', plot = fig2, device = NULL,
       width=5, height =3.5, units = 'in', path = NULL,
       scale=1, dpi = 300, limitsize = TRUE)

top_10<-dat %>%
  group_by(facility_outfall) %>%
  summarise(median_load=median(kg_N_TN_per_month_complete)) %>%
  arrange(desc(median_load)) %>%
  filter(median_load>=22507)

top_10$facility_outfall

top_10_dat<-dat %>% 
  filter(facility_outfall %in% top_10$facility_outfall)


fig3<-ggplot(top_10_dat, aes(x=ym(month_year), y=kg_N_TN_per_month_complete/1000)) +
  geom_point(aes(col=as.factor(imputed_missing_value)),  shape = 20)+
  ylab(expression(paste(
    'N load (Mg N',~mo^-1,')')))+
  xlab('Date')+
  scale_x_date(breaks=as.Date(x=c("1995-01-01", "2005-01-01", "2015-01-01"),
                              format = "%Y-%m-%d"), 
               date_labels='%Y')+
  theme_minimal()+
  scale_color_manual(name="data origin", 
                     labels=c("original value", "imputed value"), 
                     values = c('dodgerblue4','tomato4'))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~facility_outfall, scale="free_y", labeller = label_wrap_gen(20))
fig3

ggsave('fig3.jpeg', plot = fig3, device = NULL,
       width=8, height =4, units = 'in', path = NULL,
       scale=1, dpi = 300, limitsize = TRUE)


