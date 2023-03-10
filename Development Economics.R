## importing land records dataset
dilrpmp <- read_excel("Downloads/dilrpmp.xlsx")

## importing good governance dataset
ggi2019 <- read_excel("Downloads/ggi2019.xlsx")

## unfortunately the given data sets have different state names so they cannot be merged properly. wow
ggi2019$state <- dilrpmp$state

## merging datasets
library(dplyr)
merged_dataset <- merge(dilrpmp, ggi2019, by="state", all=TRUE)

## computing per village number of total maps, good condition maps and digitalised maps
merged_dataset$pv_map <- (merged_dataset$map_total/merged_dataset$total_villages)
merged_dataset$pv_goodmap <- (merged_dataset$map_goodcondition/merged_dataset$total_villages)
merged_dataset$pv_digitisedmap <- (merged_dataset$map_digitized/merged_dataset$total_villages)

## computing per village allocation under CLR and survey
merged_dataset$pv_clrallocation <- (merged_dataset$CLR_allocation/merged_dataset$total_villages)
merged_dataset$pv_surveyallocation <- (merged_dataset$survey_allocation/merged_dataset$total_villages) 

## computing share of villages with completed land digitisation as a percentage
merged_dataset$share_clrcompleted <- ((merged_dataset$CLR_completed/merged_dataset$total_villages)*100)

## DATA ANALYSIS 

## adding histograms
library(ggplot2)
ggplot(merged_dataset, aes((pv_map))) + geom_histogram(binwidth=30, colour="black", fill="cyan") + ggtitle("pv_map histogram") + xlab("per village total number of maps")  
ggplot(merged_dataset, aes(x=pv_goodmap)) + geom_histogram(binwidth=30, colour="black", fill="pink") + ggtitle("pv_goodmap histogram") + xlab("per village number of good condition maps")
ggplot(merged_dataset, aes(x=pv_digitisedmap)) + geom_histogram(binwidth=30 ,colour="black", fill="gold") + ggtitle("pv_digitsedmap histogram") +xlab("per village number of digitalised maps")

## adding the scatterplot with regression line
ggplot(merged_dataset, aes(x=composite_points, y=pv_map)) + geom_point(colour="blue") +geom_smooth(method=lm, colour="black")

## regression. finally
reg_1.3    <- lm(merged_dataset$pv_map ~ merged_dataset$composite_points + merged_dataset$total_villages)
reg_1.6.1  <- lm(merged_dataset$pv_goodmap ~ merged_dataset$composite_points + merged_dataset$total_villages)
reg_1.6.2  <- lm(merged_dataset$pv_digitisedmap ~ merged_dataset$composite_points + merged_dataset$total_villages)
reg_1.7    <- lm(merged_dataset$share_clrcompleted ~ merged_dataset$composite_points + merged_dataset$total_villages)
reg_1.9    <- lm(merged_dataset$share_clrcompleted ~ merged_dataset$infra_points + merged_dataset$econ_points + merged_dataset$total_villages)
reg_1.11.1 <- lm(merged_dataset$pv_clrallocation ~ merged_dataset$infra_points + merged_dataset$econ_points + merged_dataset$total_villages)
reg_1.11.2 <- lm(merged_dataset$pv_surveyallocation ~ merged_dataset$infra_points + merged_dataset$econ_points + merged_dataset$total_villages)

