

library(quantmod)
library(dplyr)
library(PerformanceAnalytics)

silver <- getSymbols('SLV', auto.assign = F)

silver2 <- as.tbl(data.frame(index(silver),
                             coredata(silver$SLV.Close),
                             log(coredata(silver$SLV.Close))))

colnames(silver2) <- c("date", "close", "log_close")

silver2 <- silver2 %>% mutate(pct_chg = 100 * (log_close - Lag(log_close)))
                                    
# big_moves <- silver.log %>% filter(abs(pct_chg) >= 3)

test <- silver2 %>% mutate(big_move = cut(silver2$pct_chg, breaks = c(-3, 0, 3),
                                          labels = c('Large Decline',
                                                     'Small Decline',
                                                     'Small Gain',
                                                     'Large Gain')))

silver.log <- silver.log %>% mutate(big_move = abs(pct_chg) >= 3)

silver.log <- silver.log %>% mutate(maxup = rollmax(silver.log$close, 15, align = 'left', na.pad = T))
silver.log <- silver.log %>% mutate(maxdown = -1 * rollmax(-silver.log$close, 15, align = 'left', na.pad = T))

silver.log <- silver.log %>% mutate(log_maxup = log(silver.log$maxup))
silver.log <- silver.log %>% mutate(log_maxdown = log(silver.log$maxdown))

silver.log <- silver.log %>% mutate(max_up_pct = 100 * (silver.log$log_close))