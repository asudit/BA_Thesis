rm(list=ls())
setwd("~/Users/Adam/Research/BA_Thesis/Data")
library("openxlsx")
library(plm)
library(foreign, gplots)
library(dplyr)
library(lazyeval) #for the group by function
library(ggplot2)
#library(micEconCES)
library(logistf)
library(stargazer)
library(biglm)

#panel_original <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data final/regression_data edits ok.xlsx", 1)
panel_original <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data final/regression_data_changedIV.xlsx", 1)

####################### merge the other debt figure from Rajan - ugh! ##############
debt_path = '/Users/Adam/Research/BA_Thesis/Data final/table_10 -- includes debt per acre in 1920.dta'
rajan_data <- read.dta(debt_path)
rajan_data <- data.frame(rajan_data[c('fips', 'debt_acre_1920')])

panel_original$fips <- panel_original$FIPS.code
library(sqldf)
panel_original <- sqldf('Select a.*, b.debt_acre_1920 FROM
                    panel_original AS a LEFT JOIN 
                    rajan_data AS b ON (a.fips = b.fips)')

###################### merge industries with crosswalk #############################

industry_csv = '/Users/Adam/Research/BA_Thesis/Data final/Industry Crosswalk.xlsx'
industry_data = read.xlsx(industry_csv, 1)
industry_data$industry_code <- as.character(industry_data$Code)
industry_data$ext_finance <- as.numeric(industry_data$Matural.Company.Financial.Dependence)
industry_data <- data.frame(industry_data[c('industry_code', 'Industry', 'ext_finance')])

panel_original$industry_code <- as.character(panel_original$`Unnamed:.21`)

panel_original <- sqldf('Select a.*, b.Industry, b.ext_finance FROM
                    panel_original AS a LEFT JOIN 
                  industry_data AS b ON (a.industry_code = b.industry_code)')
#################################### CPI ##################
#CPI <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xlsx")
#panel_original <- sqldf('Select a.*, b.CPI FROM
                    #panel_original AS a LEFT JOIN 
                  #CPI AS b ON (a.Year = b.Year)')
################################ lets get crackin #################################

panel <- data.frame(panel_original[c('Year', 'County', 'firm.code', 'Value.of.product', 'varying_iv', 'fixed_char', 'open_29', 'Post_1929', 'debt',
                                    'Wage.earners.by.month,.January', 'Wage.earners.by.month,.February', 'Wage.earners.by.month,.March',
                                    'Wage.earners.by.month,.April', 'Wage.earners.by.month,.May', 'Wage.earners.by.month,.June',
                                    'Wage.earners.by.month,.July', 'Wage.earners.by.month,.August', 'Wage.earners.by.month,.September',
                                    'Wage.earners.by.month,.October', 'Wage.earners.by.month,.November', 'Wage.earners.by.month,.December', "Unnamed:.21"
                                    , 'debt_acre_1920', 'Industry', 'ext_finance')])
panel <- subset(panel, panel$open_29 == 1)

#duplicates section
#panel_elements <- panel[c('Year', 'firm.code')]
#dup <- data.frame(duplicated(panel_elements))
#new_panel = data.frame(panel, dup)
#panel <- new_panel[new_panel$duplicated.panel_elements. == F,]

#is.na(panel) <- sapply(panel, is.infinite)
panel[mapply(is.infinite, panel)] <- NA
# remove infintie and NAn
#panel <- panel[is.finite(rowSums(panel)),]
#panel <- panel[!rowSums(!is.finite(panel)),]

#y <- as.numeric(as.character(panel$Value.of.product))
#y[!is.na(y) & y > 0] <- log(y[!is.na(y) & y > 0])
#varying_iv <- as.numeric(panel$varying_iv)

panel$firm <- as.factor(panel$firm.code)
panel$county <- as.factor(panel$County)
panel$year <- as.factor(panel$Year)
panel$post <- as.numeric(panel$Post_1929)
panel$varying_iv <- as.numeric(panel$varying_iv)
panel$industry <- as.factor(panel$Industry)
#panel$CPI <- as.numeric(panel$CPI)

#panel$y <- y/panel$CPI

panel$varying_iv[27598] <- NA

#fixed_iv_model_y_noFixedEffects <- lm(y ~ post*fixed_char, data = panel)
#fixed_iv_model_y_YearCounty <- lm(y ~ post*fixed_char  + year + county, data = panel)
#fixed_iv_model_y <- lm(y ~ post*fixed_char  + year + county + industry, data = panel)

#library(sandwich)
#library(lmtest)
#library(multiwayvcov)
#fixed_iv_model_y_noFixedEffects <- coeftest(fixed_iv_model_y_noFixedEffects, vcov=vcovHC(fixed_iv_model_y_noFixedEffects,type="HC0",cluster="County"))
#fixed_iv_model_y_YearCounty <- coeftest(fixed_iv_model_y_YearCounty, vcov=vcovHC(fixed_iv_model_y_YearCounty,type="HC0",cluster="County"))
#fixed_iv_model_y <- coeftest(fixed_iv_model_y, vcov=vcovHC(fixed_iv_model_y,type="HC0",cluster="County"))

#varying_iv_model <-lm(y ~ varying_iv + year, data = panel)
#fixed_iv_model <- plm(y ~ post + fixed_char + post*fixed_char -1, data = panel, index = c('firm.code', 'Year'), model='within')
#varying_iv_model <-plm(y ~ varying_iv, data = panel, index = c('firm.code', 'Year'), model='within')
#summary(fixed_iv_model)
#summary(varying_iv_model)

panel$labor <- rep(NA, 34207)

panel$labor <- (as.numeric(panel$Wage.earners.by.month..January) + as.numeric(panel$Wage.earners.by.month..February) +
  as.numeric(panel$Wage.earners.by.month..March) + as.numeric(panel$Wage.earners.by.month..April)+ 
  as.numeric(panel$Wage.earners.by.month..May) + as.numeric(panel$Wage.earners.by.month..June) + 
as.numeric(panel$Wage.earners.by.month..July) + as.numeric(panel$Wage.earners.by.month..August) + 
  as.numeric(panel$Wage.earners.by.month..September) + as.numeric(panel$Wage.earners.by.month..October) 
+ as.numeric(panel$Wage.earners.by.month..November) + as.numeric(panel$Wage.earners.by.month..December))

panel$labor[!is.na(panel$labor) & panel$labor > 0] <- log(panel$labor[!is.na(panel$labor) & panel$labor > 0])
panel[mapply(is.infinite, panel)] <- NA

fixed_iv_model_no_FixedEffects <- lm(labor ~ post*fixed_char, data = panel)
fixed_iv_model_JustYearCounty <- lm(labor ~ post*fixed_char + year + county, data = panel)
fixed_iv_model_labor <- lm(labor ~ post*fixed_char  + year + industry + county, data = panel)
summary(fixed_iv_model_labor)
varying_iv_model_labor <- lm(labor ~ varying_iv  + year + industry + county, data = panel)
summary(varying_iv_model_labor)

library(sandwich)
library(lmtest)
library(multiwayvcov)
fixed_iv_model_labor <- coeftest(fixed_iv_model_labor, vcov=vcovHC(fixed_iv_model_labor,type="HC0",cluster="County"))
fixed_iv_model_no_FixedEffects <- coeftest(fixed_iv_model_no_FixedEffects, vcov=vcovHC(fixed_iv_model_no_FixedEffects,type="HC0",cluster="County"))
fixed_iv_model_JustYearCounty <- coeftest(fixed_iv_model_JustYearCounty, vcov=vcovHC(fixed_iv_model_JustYearCounty,type="HC0",cluster="County"))
varying_labor_robust <- coeftest(varying_iv_model_labor, vcov=vcovHC(varying_iv_model_labor,type="HC0",cluster="County"))

################################# make iv for robustness check  ############

panel_robust  <-subset(panel, panel$debt != "")
# no NA's for two stage
panel_robust <- panel_robust[complete.cases(panel_robust),] #5:6

panel_robust$debt <- as.numeric(panel_robust$debt)
#mortgage_debt[!is.na(mortgage_debt) & mortgage_debt > 0] <- log(mortgage_debt[!is.na(mortgage_debt) & mortgage_debt > 0])
panel_robust$debt_acre_1910 <- abs(panel_robust$debt - panel_robust$debt_acre_1920)
panel_robust$debt_normalized <- panel_robust$debt_acre_1920/panel_robust$debt_acre_1910


SLS1 <- lm(post*fixed_char ~ debt + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS1)
X_hat <- fitted(SLS1)
SLS2_labor <- lm(labor ~ X_hat + year + county + industry, data = panel_robust, na.action=na.omit)

summary(SLS2_labor)

SLS1_norm <- lm(post*fixed_char ~ debt_normalized + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS1_norm)
X_hat_norm <- fitted(SLS1_norm)
SLS2_labor_norm <- lm(labor ~ X_hat_norm + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS2_labor_norm)
#panel_robust$instrument <- X_hat

#sign doesnt make sense...dont use
#SLS1_varying <- lm(varying_iv ~ debt + year + county + industry, data = panel_robust, na.action=na.omit)
#summary(SLS1_varying)
#X_hat_varying <- fitted(SLS1_varying)
#SLS2_labor_varying <- lm(labor ~ X_hat_varying + year + county + industry, data = panel_robust, na.action=na.omit)

#summary(SLS2_labor_varying)

library(sandwich)
library(lmtest)
library(multiwayvcov)
SLS1_labor_robust <- coeftest(SLS1, vcov=vcovHC(SLS1,type="HC0",cluster="County"))
SLS2_labor_robust <- coeftest(SLS2_labor, vcov=vcovHC(SLS2_labor,type="HC0",cluster="County"))

#output?
#SLS1_y <- lm(post*fixed_char ~ debt + year + county + industry, data = panel_robust, na.action=na.omit)
#summary(SLS1_y)
#X_hat <- fitted(SLS1_y)
#SLS2_y <- lm(y ~ X_hat + year + county + industry, data = panel_robust, na.action=na.omit)
#summary(SLS2_y)

##second IV
#panel_robust$debt_acre_1920 <- as.numeric(panel_robust$debt_acre_1920)

#SLS1 <- lm(post*fixed_char ~ debt_acre_1920 + year + county + industry, data = panel_robust, na.action=na.omit)
#summary(SLS1)
#X_hat <- fitted(SLS1)
#SLS2_labor_IV2 <- lm(labor ~ X_hat + year + county + industry -1, data = panel_robust, na.action=na.omit)

#summary(SLS2_labor_IV2)

#library(sandwich)
#library(lmtest)
#library(multiwayvcov)
#SLS2_labor_IV2_robust <- coeftest(SLS2_labor_IV2, vcov=vcovHC(SLS2_labor_IV2,type="HC0",cluster="County"))

########################## external financing by industry ###########################
#median is 0.04
high_ext_dependence <- subset(panel_robust, panel_robust$ext_finance > 0.04)
low_ext_dependence <- subset(panel_robust, panel_robust$ext_finance <= 0.04)

high_dep_SLS1 <- lm(post*fixed_char ~ debt  + year  + county + industry -1, data = high_ext_dependence, na.action=na.omit)
summary(high_dep_SLS1)
X_hat <- fitted(high_dep_SLS1)
high_dep_SLS2 <- lm(labor ~ X_hat + year + county + industry -1, data = high_ext_dependence, na.action=na.omit)
summary(high_dep_SLS2)

library(sandwich)
library(lmtest)
library(multiwayvcov)
high_ext_dep_labor <- coeftest(high_dep_SLS2, vcov=vcovHC(high_dep_SLS2,type="HC0",cluster="County"))

low_dep_SLS1 <- lm(post*fixed_char ~ debt  + year  + county + industry -1, data = low_ext_dependence, na.action=na.omit)
summary(low_dep_SLS1)
X_hat <- fitted(low_dep_SLS1)
low_dep_SLS2 <- lm(labor ~ X_hat + year + county + industry -1, data = low_ext_dependence, na.action=na.omit)
summary(low_dep_SLS2)

library(sandwich)
library(lmtest)
library(multiwayvcov)
low_ext_dep_labor <- coeftest(low_dep_SLS2, vcov=vcovHC(low_dep_SLS2,type="HC0",cluster="County"))

############# WALD test #############
# get variances of each model, after clustering at county level
vcov_high=vcovHC(high_dep_SLS2,type="HC0",cluster="County")
high_var <- vcov_high[c('X_hat', 'X_hat'),c('X_hat', 'X_hat')][1,1]
high_coeff<- summary(high_dep_SLS2)$coefficients[1,]

vcov_low=vcovHC(low_dep_SLS2,type="HC0",cluster="County")
low_var <- vcov_low[c('X_hat', 'X_hat'),c('X_hat', 'X_hat')][1,1]
low_coeff<- summary(low_dep_SLS2)$coefficients[1,]

covar <- cov(high_coeff, low_coeff) #is this correct????

se <- sqrt(high_var + low_var -2*covar)
wald.z <- (summary(high_dep_SLS2)$coefficients[1,1] - summary(low_dep_SLS2)$coefficients[1,1])/se
p_wald <- 2*pnorm(wald.z)

################better approach###############
panel$dependence_dummy <- ifelse((panel$ext_finance > 0.04), 1, ifelse(panel$ext_finance <= 0.04 , 0, 0))
panel_robust$dependence_dummy <- ifelse((panel_robust$ext_finance > 0.04), 1, ifelse(panel_robust$ext_finance <= 0.04 , 0, 0))

fixed_iv_model_labor_dependence <- lm(labor ~ dependence_dummy*post*fixed_char  + year  + county + industry, data = panel)

SLS1_dependence <- lm(dependence_dummy*post*fixed_char ~ debt  + year  + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS1_dependence)
X_hat <- fitted(SLS1_dependence)
SLS2_dependence <- lm(labor ~ dependence_dummy*X_hat + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS2_dependence)

library(sandwich)
library(lmtest)
library(multiwayvcov)
fixed_iv_model_labor_dependence <- coeftest(fixed_iv_model_labor_dependence, vcov=vcovHC(fixed_iv_model_labor_dependence,type="HC0",cluster="County"))
ext_dependence_model <- coeftest(SLS2_dependence, vcov=vcovHC(SLS2_dependence,type="HC0",cluster="County"))




#WALD test etc
#dummy for high versus low - add dummy*X_hat as extra or interact with all other variables as well

#heterogeniety analysis -- bunch of figures x axis financial dependence, and y axis coefficient of interest
#

########## fun with tables #######################
#ggplot(data = panel, aes(x=panel$year, y=panel$labor))  + geom_point(aes(colour = factor(panel$industry)), size = 4)  + xlab("Year") + ylab("Labor") + ggtitle("Scatterplot of Labor During Great Depression by industry")

#ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Capital") + ggtitle("Scatterplot of Capital vs. Bank Distress (with OLS fit line)")
#ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.value.of.products))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Output") + ggtitle("Scatterplot of Output vs. Bank Distress (with OLS fit line)")
table1 <- stargazer(fixed_iv_model_no_FixedEffects, fixed_iv_model_JustYearCounty,
                    fixed_iv_model_labor,title="Fixed Effects", align=TRUE)
table2 <- stargazer(SLS1_labor_robust, SLS2_labor_robust, title="Instrumental Variables", align=TRUE)
table3 <- stargazer(fixed_iv_model_labor_dependence, title="Dependence on External Finance", align=TRUE)
#table3 <- stargazer(fixed_iv_model_y_noFixedEffects, fixed_iv_model_y_YearCounty,
                    #fixed_iv_model_y,title="Fixed Effects -- Total Value Added", align=TRUE)

################################################### fun with graphs ##############################################

############### spatial map #################################
install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")

#make sure the packages are running
require(maps)
require(ggmap)
par(mfrow = c(2, 1))
#map("usa")
data(county.fips)
# Plot unemployment by country
#colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
colors = c("#C994C7", "#DF65B0", "#DD1C77", "#980043")


maps_data <- panel_original[c('fips', 'fixed_char')]
maps_data$log_bank_distress <- abs(as.numeric(log(maps_data$fixed_char)))
maps_data$log_bank_distress[!is.na(maps_data$log_bank_distress) & maps_data$log_bank_distress > 0] <- log(maps_data$log_bank_distress[!is.na(maps_data$log_bank_distress) & maps_data$log_bank_distress > 0])
maps_data[mapply(is.infinite, maps_data)] <- 0

maps_data$colorBuckets <- as.numeric(cut(maps_data$log_bank_distress, breaks = 4))
colorsmatched <- maps_data$colorBuckets[match(county.fips$fips, maps_data$fips)]
colorsmatched[is.na(colorsmatched)] <- 1

map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,  lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection = "polyconic")
title("Bank Distress by County, 1929-1935")

leg.txt <- c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile")
legend("top", leg.txt,horiz = TRUE,fill = colors, cex = 0.45)


######################## changes in revenue and labor by industry graphs ################
CPI <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xlsx")
panel_changes <- sqldf('Select a.*, b.CPI FROM panel_original AS a LEFT JOIN CPI AS b ON (a.Year = b.Year)')
panel_changes <- subset(panel_changes, panel_changes$open_29 == 1 & panel_changes$open_33 ==1 & Year!=1935)

panel_changes$CPI <- as.numeric(panel_changes$CPI)
y <- as.numeric(as.character(panel_changes$Value.of.product))
#y[!is.na(y) & y > 0] <- log(y[!is.na(y) & y > 0])
panel_changes$y <- y/panel_changes$CPI
panel_changes$y <- panel_changes$y * 1/1000
#panel_original_revenue[mapply(is.infinite, panel_original_revenue)] <- NA
#panel_changes[is.na(panel_changes)] <- 0
#panel_original_revenue$dependence_dummy <- ifelse((panel_original_revenue$ext_finance > 0.04), 1, ifelse(panel_original_revenue$ext_finance <= 0.04 , 0, 0))
panel_changes$labor <- (as.numeric(panel_changes[,'Wage.earners.by.month,.January']) + as.numeric(panel_changes[,'Wage.earners.by.month,.February']) +
                  as.numeric(panel_changes[,'Wage.earners.by.month,.March']) + as.numeric(panel_changes[,'Wage.earners.by.month,.April'])+ 
                  as.numeric(panel_changes[,'Wage.earners.by.month,.May']) + as.numeric(panel_changes[,'Wage.earners.by.month,.June']) + 
                  as.numeric(panel_changes[,'Wage.earners.by.month,.July']) + as.numeric(panel_changes[,'Wage.earners.by.month,.August']) + 
                  as.numeric(panel_changes[,'Wage.earners.by.month,.September']) + as.numeric(panel_changes[,'Wage.earners.by.month,.October']) 
                + as.numeric(panel_changes[,'Wage.earners.by.month,.November']) + as.numeric(panel_changes[,'Wage.earners.by.month,.December']))
panel_changes$labor <- panel_changes$labor * 1/1000

#rev_data <- data.frame(ddply(panel_original_revenue, .(Industry, Year,ext_finance), summarize,  Average_rev=mean(y)))
#rev_data$dependence_dummy <- as.integer(rev_data$dependence_dummy)
#rev_data$Industry <- as.factor(rev_data$Industry)
#rev_data$Year <- as.integer(rev_data$Year)

#ggplot(data =subset(rev_data, ext_finance < -0.10 & ext_finance >= -0.12), aes(x=Year, y=Average_rev, group=Industry, shape=Industry, color=Industry)) +geom_line() +  geom_point()+ xlab("Year") + ylab("Revenue, Adjusted for Inflation (hundred thousands)") + ggtitle("Average Firm Revenue by Industry, 1929-1935")
#ggplot(data =subset(rev_data, ext_finance == -0.10), aes(x=Year, y=Average_rev, group=Industry, shape=Industry, color=Industry)) +geom_line() +  geom_point()+ xlab("Year") + ylab("Revenue, Adjusted for Inflation (hundred thousands)") + ggtitle("Average Firm Revenue by Industry, 1929-1935")
#ggplot(data =subset(rev_data, ext_finance <= 0.14 & ext_finance > -0.10), aes(x=Year, y=Average_rev, group=Industry, shape=Industry, color=Industry)) +geom_line() +  geom_point()+ xlab("Year") + ylab("Revenue, Adjusted for Inflation (hundred thousands)") + ggtitle("Average Firm Revenue by Industry, 1929-1935")
#ggplot(data =subset(rev_data, ext_finance <=0.39 & ext_finance > 0.14), aes(x=Year, y=Average_rev, group=Industry, shape=Industry, color=Industry)) +geom_line() +  geom_point()+ xlab("Year") + ylab("Revenue, Adjusted for Inflation (hundred thousands)") + ggtitle("Average Firm Revenue by Industry, 1929-1935")

#ggplot(data =subset(rev_data, ext_finance< 0.4), aes(x=Industry, y=Average_rev, fill=Industry)) + xlab("Industry") + ylab("Revenue, Adjusted for Inflation (hundred thousands)") + ggtitle("Average Firm Revenue by Industry, 1929-1935") +geom_bar(stat="identity", position=position_dodge())
panel_changes<-panel_changes[order(panel_changes$firm.code,panel_changes$Year),]
panel_changes$y_diff <-panel_changes$y -  lag(lag(panel_changes$y))
panel_changes$l_diff <- panel_changes$labor - lag(lag(panel_changes$labor))


#View(Panel_diff)

for (i in 3:nrow(panel_changes)){
  if (panel_changes$firm.code[i]!=panel_changes$firm.code[i-2]) {
    panel_changes$y_diff[i] <- NA 
    panel_changes$l_diff[i] <- NA 
  }
}
panel_changes[is.na(panel_changes)] <- 0
rev_change_data <- data.frame(ddply(panel_changes, .(Industry), summarize,  Average_change=mean(y_diff)))
ggplot(data =rev_change_data, aes(x=Industry, y=Average_change, fill=Industry)) + xlab("Industry") + ylab("Average Change in Revenue, 1933-1929, Adjusted for Inflation (in thousands)") + ggtitle("Average Change in Revenue by Industry, 1933-1929") +geom_bar(stat="identity", position=position_dodge(0.9)) + geom_text(aes(label=Industry), size = 3,angle = 90) + theme(axis.text.x=element_blank())
                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                              

labor_change_data <- data.frame(ddply(panel_changes, .(Industry), summarize,  Average_change=mean(l_diff)))
ggplot(data =labor_change_data, aes(x=Industry, y=Average_change, fill=Industry)) + xlab("Industry") + ylab("Average Change in Wage Earners Hired, 1933-1929 (in thousands)") + ggtitle("Average Change in Employment by Industry, 1933-1929") +geom_bar(stat="identity", position=position_dodge(0.9)) + geom_text(aes(label=Industry), size = 3,angle = 90) + theme(axis.text.x=element_blank())

################## firm exit graph #########
panel_exit <- panel_original
est_change_data <- data.frame(ddply(subset(panel_exit, open_29 ==1),~Year,summarise,est_count=length(unique(firm.code))))
entry_29 <- length(unique(subset(panel_exit, panel_exit$open_29 ==1)$firm.code))
entry_31 <- length(unique(subset(panel_exit, panel_exit$open_29 ==0 & panel_exit$open_31 ==1 )$firm.code))
entry_33 <- length(unique(subset(panel_exit, panel_exit$open_29 ==0 & panel_exit$open_31 ==0 &panel_exit$open_33 ==1)$firm.code))
entry_35 <- length(unique(subset(panel_exit, panel_exit$open_29 ==0 & panel_exit$open_31 ==0 &panel_exit$open_33 ==0 & panel_exit$open_35 ==1)$firm.code))
entry <- c(entry_29, entry_31, entry_33, entry_35)
year_entry <- c(1929, 1931, 1933, 1935)
entry_data <- data.frame(entry, year_entry)

ggplot(data =est_change_data, aes(x=Year, y=est_count)) +geom_line() +  geom_point()+ xlab("Year") + ylab("Number of Establishments") + ggtitle("Number of Firms Per Year, 1929-1935") 

################### Coefficient by industry #####################
library(sandwich)
library(lmtest)
library(multiwayvcov)
d <- data.frame(matrix(NA, nrow = 22, ncol = 3))
for(i in 1:length(levels(panel$industry)))
  {
  new_industry <- levels(panel$industry)[i]
  fixed_iv_model_labor <- lm(labor ~ post*fixed_char  + year + county, data = subset(panel, panel$industry == new_industry))
  fixed_iv_model_labor <- coeftest(fixed_iv_model_labor, vcov=vcovHC(fixed_iv_model_labor,type="HC0",cluster="County"))
  #coeff<- tail(fixed_iv_model_labor[,1],1)
  #print(c(levels(panel$industry)[i], coeff))
  coeff<- fixed_iv_model_labor[3,1]
  d[i,] <- c(levels(panel$industry)[i], as.numeric(as.character(coeff)), head(panel$ext_finance[panel$industry == new_industry]))

}
d$coeff <- as.numeric(d$X2)
ggplot(data =d, aes(x=X3, y=coeff, group=1)) +  geom_point()+ geom_smooth(method='lm')+ xlab("External Financial Dependence") + ylab("Bank Distress Coefficient") + ggtitle("Effects of Bank Distress on Employment v. External Financial Dependence by Industry") 

########################## Summary Statistics #####################

############ banking stats ##################
fdic_data <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/FDIC_check.xlsx")
fdic_data$Year <- as.integer(fdic_data$Year)
fdic_data <- subset(fdic_data, Year == 1929 |Year == 1930 | Year == 1931 | Year == 1932 |Year == 1933)

central_data <- subset(fdic_data, State == 'ohio' | State == 'illinois' | State =='indiana' | State == 'michigan' | State=='wisconsin')
mid_atlantic_data <- subset(fdic_data, State == 'new york' | State == 'new jersey' | State =='pennsylvania')
mountain_data <- subset(fdic_data, State == 'montana' | State == 'idaho' | State =='wyoming' | State == 'colorado' | State=='new mexico' | State=='arizona' | State=='utah'| State=='nevada')
new_england_data <- subset(fdic_data, State == 'maine' | State == 'new hampshire' | State =='vermont' | State == 'massachusetts' | State=='rhode island' | State=='connecticut')
northwestern_data <- subset(fdic_data, State == 'minnesota' | State == 'iowa' | State =='missouri' | State == 'north dakota' | State=='south dakota' | State=='nebraska' | State=='kansas')
pacific_data <- subset(fdic_data, State == 'washington' | State == 'oregon' | State =='california')
south_atlantic_data <- subset(fdic_data, State == 'maryland' | State == 'delaware' | State =='district of columbia'| State =='virginia'| State =='west virginia' | State =='north carolina'| State =='south carolina'| State =='georgia'| State =='florida')
south_central_data <- subset(fdic_data, State == 'kentucky' | State == 'tennessee' | State =='alabama' | State =='mississippi' | State =='arkansas'| State =='oklahoma'| State =='louisiana'| State =='texas')

central_data <- unique(central_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
mid_atlantic_data <- unique(mid_atlantic_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
mountain_data <- unique(mountain_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
new_england_data <- unique(new_england_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
northwestern_data <- unique(northwestern_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
pacific_data <- unique(pacific_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
south_atlantic_data <- unique(south_atlantic_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])
south_central_data <- unique(south_central_data[c('State', 'County', 'Year', 'FDIC_BANKS_SUS_','FDIC.DEPOSITS','FDIC_DEPOSITS_SUS_','FDIC.BANKS')])



central_data_sum <- data.frame(ddply(central_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
mid_atlantic_data_sum <- data.frame(ddply(mid_atlantic_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
mountain_data_sum <- data.frame(ddply(mountain_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
new_england_sum <- data.frame(ddply(new_england_data, .(Year), summarize, banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
northwestern_sum <- data.frame(ddply(northwestern_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
pacific_sum <- data.frame(ddply(pacific_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
south_atlantic_sum <- data.frame(ddply(south_atlantic_data, .(Year), summarize, banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))
south_central_sum <- data.frame(ddply(south_central_data, .(Year), summarize,  banks_sus=sum(FDIC_BANKS_SUS_),banks=sum(FDIC.BANKS),deposits_sus=sum(FDIC_DEPOSITS_SUS_),deposits=sum(FDIC.DEPOSITS), bank_percentage=banks_sus/banks, deposits_percentage = deposits_sus/deposits))

####################### census stats ########################
count(unique(panel_original$firm.code))
count(unique(panel_original[c('County','State')]))
