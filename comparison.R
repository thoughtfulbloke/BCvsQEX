library(dplyr)
library(tidyr)
library(ggplot2)
# Group: Earnings and Employment Survey (QES) - QEX Infoshare statsNZ
# Table: Full-Time Equivalent Employees by Industry (ANZSIC06) and Sex (Qrtly-Mar/Jun/Sep/Dec)
# Retail, construction, Manufacturing, total

QEX <- read.csv("QEX.csv", stringsAsFactors = FALSE, skip=1) %>% slice(-1)

# QEX is quarterly, so to make it look like the graphs people visually compare, 
# I am filling in the missing months with the idea that the quarter is the end month figure
FTES <- QEX %>%
  mutate(MFCT = as.numeric(Manufacturing),
         CONS = as.numeric(Construction),
         RTL = as.numeric(Retail.Trade),
         TOT = as.numeric(Total.All.Industries),
         OF_INTEREST = TOT,
         PREVIOUS= lag(OF_INTEREST),
         imputation = (TOT - PREVIOUS)/3,
         month1 = PREVIOUS + imputation,
         month2 = OF_INTEREST - imputation,
         month3 = OF_INTEREST) %>% 
  select(X, month1, month2, month3) %>%
  gather(Month, FTE, month1:month3) %>% 
  filter(!is.na(FTE)) %>% arrange(X, Month) %>%
  mutate(growthFTE = 100 * FTE/lag(FTE,12) - 100)

# Similarly, because the ANZ Outlook survey does not run in Janaury, but people looking 
# at charts see a line, I have added JAnuary points halfway between December and February

ANZ <- read.csv("ANZemploy.csv", 
                colClasses = c("Date", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric")) %>% arrange(MONTH)

# combine the data 
EMP <- bind_cols(ANZ[1:320,], FTES[48:367,])
cast <- -36:36

cor_offset <- function(offset, dataset=EMP[,c("TOT","growthFTE")]){
  if (offset < 0){
    dataset$offset <- lag(dataset$growthFTE,abs(offset)) #predicting the past
  }
  if (offset > 0){
    dataset$offset <- lead(dataset$growthFTE,offset) #predicting the past
  }
  if(offset == 0) {
    dataset$offset <- dataset$growthFTE
  }
  dataset <- dataset[complete.cases(dataset),c("TOT","offset")]
  cor(dataset$TOT,dataset$offset)
}
matches <- sapply(cast, cor_offset)



ggplot(data=data.frame(months_foward_looking= cast, correlation=matches),
       aes(x=months_foward_looking, y=correlation)) + geom_line() +geom_point() +
  theme_minimal()

dataset <- EMP[,c("MONTH", "TOT","growthFTE")]
dataset$bestoffset <- lead(dataset$growthFTE,8)
dataset <- dataset[complete.cases(dataset),]
linear_model <- lm(bestoffset ~ TOT, data=dataset)
dataset$wrongness_of_prediction <- linear_model$fitted.values - dataset$bestoffset
ggplot(data=dataset,
       aes(x=MONTH, y=wrongness_of_prediction)) + geom_line() +geom_point() +
  theme_minimal() + ylab("Business overly pessimistic  -  Business overly optimistic") +
  ylim(-4,4) + geom_hline(yintercept=0, colour="red")
