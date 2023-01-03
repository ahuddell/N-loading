
x <- c(1,2,3,4,NA,NA,NA,NA,NA,NA,10,11,12,
       101,102,103,104,NA,NA,NA,NA,NA,NA,110,111,112)
#date <- Sys.Date() + (1:20) #time series
group <-  rep(c("a", "b"), each = 13)


test_df <- data.frame(group, x)  %>%
  mutate(consecutive_imputes=(c(0,0,0,0,6,6,6,6,6,6,0,0,0,
                                0,0,0,0,6,6,6,6,6,6,0,0,0))) %>%
  group_by(group) 

test_df %>%
  mutate(test=runner(x = x, 
        # at=consecutive_imputes>5,
         k=3,
    #  f = function(x) mean(x, na.rm = TRUE)
      f = function(x) sample(x, size=1)
      
  )
  ) %>%
  print(n=26)
    

#########################
  mutate(
    y = runner(
      x = ., 
  #    idx = "date", 
      k = 2,
      f=function(x) sample_n(x, size=20)
      #f = sample_n(., size=1,replace = F)
      )
  )

runner(fts2, 
       k = fts2$consecutive_imputes*2,
       at=case_when(fts2$consecutive_imputes>5),
       f =function(x) length(unique(x)))
