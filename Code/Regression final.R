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

y <- as.numeric(as.character(panel$Value.of.product))
y[!is.na(y) & y > 0] <- log(y[!is.na(y) & y > 0])
#varying_iv <- as.numeric(panel$varying_iv)

panel$firm <- as.factor(panel$firm.code)
panel$county <- as.factor(panel$County)
panel$year <- as.factor(panel$Year)
panel$post <- as.numeric(panel$Post_1929)
panel$varying_iv <- as.numeric(panel$varying_iv)
panel$industry <- as.factor(panel$Industry)

panel$varying_iv[27598] <- NA

fixed_iv_model <- lm(y ~ post + fixed_char + post*fixed_char  + year + county + industry -1, data = panel)
varying_iv_model <-lm(y ~ varying_iv + year -1, data = panel)
#fixed_iv_model <- plm(y ~ post + fixed_char + post*fixed_char -1, data = panel, index = c('firm.code', 'Year'), model='within')
#varying_iv_model <-plm(y ~ varying_iv, data = panel, index = c('firm.code', 'Year'), model='within')
summary(fixed_iv_model)
summary(varying_iv_model)

panel$labor <- rep(NA, 34207)

panel$labor <- (as.numeric(panel$Wage.earners.by.month..January) + as.numeric(panel$Wage.earners.by.month..February) +
  as.numeric(panel$Wage.earners.by.month..March) + as.numeric(panel$Wage.earners.by.month..April)+ 
  as.numeric(panel$Wage.earners.by.month..May) + as.numeric(panel$Wage.earners.by.month..June) + 
as.numeric(panel$Wage.earners.by.month..July) + as.numeric(panel$Wage.earners.by.month..August) + 
  as.numeric(panel$Wage.earners.by.month..September) + as.numeric(panel$Wage.earners.by.month..October) 
+ as.numeric(panel$Wage.earners.by.month..November) + as.numeric(panel$Wage.earners.by.month..December))

panel$labor[!is.na(panel$labor) & panel$labor > 0] <- log(panel$labor[!is.na(panel$labor) & panel$labor > 0])
panel[mapply(is.infinite, panel)] <- NA

fixed_iv_model_labor <- lm(labor ~ post*fixed_char  + year + industry + county -1, data = panel)
summary(fixed_iv_model_labor)

library(sandwich)
library(lmtest)
library(multiwayvcov)
fixed_labor_robust <- coeftest(fixed_iv_model_labor, vcov=vcovHC(fixed_iv_model_labor,type="HC0",cluster="County"))

################################# make iv for robustness check  ############

panel_robust  <-subset(panel, panel$debt != "")
# no NA's for two stage
panel_robust <- panel_robust[complete.cases(panel_robust),] #5:6

panel_robust$debt <- as.numeric(panel_robust$debt)
#mortgage_debt[!is.na(mortgage_debt) & mortgage_debt > 0] <- log(mortgage_debt[!is.na(mortgage_debt) & mortgage_debt > 0])

SLS1 <- lm(post*fixed_char ~ debt + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS1)
X_hat <- fitted(SLS1)
SLS2_labor <- lm(labor ~ X_hat + year + county + industry -1, data = panel_robust, na.action=na.omit)

summary(SLS2_labor)

library(sandwich)
library(lmtest)
library(multiwayvcov)
SLS2_labor_robust <- coeftest(SLS2_labor, vcov=vcovHC(SLS2_labor,type="HC0",cluster="County"))

##second IV
panel_robust$debt_acre_1920 <- as.numeric(panel_robust$debt_acre_1920)

SLS1 <- lm(post*fixed_char ~ debt_acre_1920 + year + county + industry, data = panel_robust, na.action=na.omit)
summary(SLS1)
X_hat <- fitted(SLS1)
SLS2_labor_IV2 <- lm(labor ~ X_hat + year + county + industry -1, data = panel_robust, na.action=na.omit)

summary(SLS2_labor_IV2)

library(sandwich)
library(lmtest)
library(multiwayvcov)
SLS2_labor_IV2_robust <- coeftest(SLS2_labor_IV2, vcov=vcovHC(SLS2_labor_IV2,type="HC0",cluster="County"))

########################## external financing by industry ###########################
#median is 0.04
high_ext_dependence <- subset(panel_robust, panel_robust$ext_finance > 0.04)
low_ext_dependence <- subset(panel_robust, panel_robust$ext_finance <= 0.04)

high_dep_SLS1 <- lm(post*fixed_char ~ debt  + year  + county + industry -1, data = high_ext_dependence, na.action=na.omit)
summary(high_dep_SLS1)
X_hat <- fitted(SLS1)
SLS2_labor <- lm(labor ~ X_hat + year + county + industry -1, data = panel_robust, na.action=na.omit)


low_dep_labor <- lm(labor ~ post*fixed_char  + year + industry + county -1, data = low_ext_dependence)
summary(low_dep_labor)

#WALD test etc


########## fun with plots #######################
#ggplot(data = panel, aes(x=panel$year, y=panel$labor))  + geom_point(aes(colour = factor(panel$industry)), size = 4)  + xlab("Year") + ylab("Labor") + ggtitle("Scatterplot of Labor During Great Depression by industry")

#ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Capital") + ggtitle("Scatterplot of Capital vs. Bank Distress (with OLS fit line)")
#ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.value.of.products))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Output") + ggtitle("Scatterplot of Output vs. Bank Distress (with OLS fit line)")


