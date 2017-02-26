#dont forget to delete inf!
rm(list=ls())
setwd("~/Users/Adam/Research/BA_Thesis/Data")
library("openxlsx")
library(plm)
library(foreign, gplots)
library(dplyr)
library(lazyeval) #for the group by function
library(ggplot2)
library(micEconCES)
library(logistf)
library(stargazer)
#Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx", 1)
#Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var_auto.xlsx", 1)
Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx", 1)
CPI <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xls")


panel_elements <- Panel[c('Year', 'ID.code')]
dup <- data.frame(duplicated(panel_elements))

new_panel = data.frame(Panel, dup)
Panel <- new_panel[new_panel$duplicated.panel_elements. == F,]
Panel[Panel==''] <- NA
#Panel<-Panel[order(Panel$ID.code,Panel$Year),]
#Panel1 <- group_by(Panel, 'Id.code')

#Panel <- lapply(Panel, function(x) x[is.finite(x)])

#for(i in 1:ncol(Panel)){
  #aa <- sapply(Panel[[i]], function(x){return(is.finite(x))})
#}
#Panel$noInf <- aa
#Panel <- Panel[Panel['noInf'] == T,]

Panel$bank_sus_norm <- as.numeric(Panel$bank_sus_norm)
var_interest <- Panel$bank_sus_norm
#var_interest <- as.numeric(var_interest)

alt_iv = as.numeric(Panel$alt_iv)
Panel$Total.value.of.products <- as.numeric(Panel$Total.value.of.products)
val_added  <- Panel$Total.value.of.products
ID <- Panel$ID.code
Year <- Panel$Year
county <- Panel$County
subsidiary <- Panel$Branch.or.subsidiary.of.other.firm

Panel$Wage.earners.by.months..total <- as.numeric(Panel$Wage.earners.by.months..total)
labor <- Panel$Wage.earners.by.months..total
#fixed_capital <- Panel$Total.HP
#variable_capital <- Panel$Cost.of.all.materials.and.raw.stock.actually.used
Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003. <- as.numeric(Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)
capital <- Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
#Nanda gets the credit
Panel$Post_1929<- as.numeric(Panel$Post_1929)
Post <- Panel$Post_1929
industry <- Panel$Industry


#issue -- the merge with FDIC did not work as well as hoped. lots of blanks

# this using built in fixed effects
#fixed <- plm(val_added ~  Post*var_interest  +  labor + capital, data=Panel, index=c('County', 'Year'), model="within")

#this is using my own fixed effects
#county <-as.factor(county)
#Year <- as.factor(Year)
fixed <- lm(val_added ~ Post*var_interest + labor + capital + factor(county) + factor(Year)  -1 , data=Panel)
fixed_alt_iv <- lm(val_added ~ Post*alt_iv + labor + capital + factor(county) + factor(Year) -1 , data=Panel)
fixed_alt_iv_lag <- lm(val_added ~ Post*Panel$alt_iv_lag + labor + capital + factor(county) + factor(Year) -1 , data=Panel)

robust_check <- lm(alt_iv ~ val_added + labor + capital + factor(county) + factor(Year) -1 , data=Panel)

summary(fixed)
summary(fixed_alt_iv)
summary(fixed_alt_iv_lag)
summary(robust_check)

fixed_external <- lm(val_added ~ Post*var_interest + labor + capital + factor(county) + factor(Year) -1 + Panel$Branch.or.subsidiary.of.other.firm.1, data=Panel)
summary(fixed_external)

#cluster standard errors
library(sandwich)
library(lmtest)
library(multiwayvcov)
#cov1 <- vcovHC(fixed, type = "HC1")
#robust_se1    <- sqrt(diag(cov1))
#cov2 <- vcovHC(fixed_alt_iv, type = "HC1")
#robust_se2    <- sqrt(diag(cov2))
vcov_year <- cluster.vcov(fixed, Panel$County) #adjust variance covariance matrix via clustering
vcov_year_alt <- cluster.vcov(fixed_alt_iv, Panel$County)

fixed_robust <- coeftest(fixed, vcov_year)
#run t test on model, with clustering covar matrix
#alt_iv_robust <- coeftest(fixed_alt_iv, vcov_year_alt)
#just to check roughly the same
alt_iv_robust <- coeftest(fixed_alt_iv, vcov=vcovHC(fixed_alt_iv,type="HC0",cluster="county"))
#####
summary(fixed_robust)

#vcov_year_check <- cluster.vcov(robust_check, Panel$County)
#robust_check_clustered <- coeftest(robust_check, vcov_year_check)
robust_check_clustered<- coeftest(robust_check, vcov=vcovHC(robust_check,type="HC0",cluster="county"))

#summary(fixed_alt_iv)
#labor_elasticity <- lm(labor ~ var_interest)
labor_elasticity <- lm(labor ~ Panel$alt_iv)
summary(labor_elasticity)
#cap_elasticity <- lm(capital ~ var_interest)
cap_elasticity <- lm(capital ~ Panel$alt_iv)
summary(cap_elasticity)
output_elasticity <- lm(Panel$Total.value.of.products ~ Panel$alt_iv)
summary(output_elasticity)
#random <- plm(val_added ~ banks_sus_ratio + labor + capital, data=Panel, index=c('ID.code', 'Year'), model="random")


###############################################################################################
########################## lagging code - first differences ##################
Panel$ID.code <- as.factor(Panel$ID.code)

Panel<-Panel[order(Panel$ID.code,Panel$Year),]
#Panel$y_diff <- lag(Panel$Total.value.of.products)
Panel$y_diff <-Panel$Total.value.of.products -  lag(Panel$Total.value.of.products)


#View(Panel_diff)

for (i in 2:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-1]) {
    Panel$y_diff[i] <- NA 
  }
}

#Panel$k_diff <- lag(Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)
Panel$k_diff <- Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003. - lag(Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)
for (i in 2:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-1]) {
    Panel$k_diff[i] <- NA 
  }
}

#Panel$l_diff <- lag(Panel$Wage.earners.by.months..total)
Panel$l_diff <- Panel$Wage.earners.by.months..total - lag(Panel$Wage.earners.by.months..total)

for (i in 2:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-1]) {
    Panel$l_diff[i] <- NA 
  }
}

#first_diff <- lm(Panel$y_diff ~ Post*var_interest + Panel$k_diff + Panel$l_diff + factor(county) + factor(Year)  , data=Panel)
first_diff <- lm(Panel$y_diff ~ Post*var_interest + Panel$k_diff + Panel$l_diff  , data=Panel)
summary(first_diff)

Panel_diff <- data.frame(Panel$y_diff, Panel$Total.value.of.products, Panel$l_diff,Panel$Wage.earners.by.months..total,  Panel$k_diff, 
                         Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)

######### "6th" differences -- lagging code#####################################################
Panel<-Panel[order(Panel$ID.code,Panel$Year),]
#Panel$y_diff <- lag(Panel$Total.value.of.products)
Panel$y_4diff <-Panel$Total.value.of.products -  lag(lag(lag(Panel$Total.value.of.products)))


#View(Panel_diff)

for (i in 4:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-3]) {
    Panel$y_4diff[i] <- NA 
  }
}

#Panel$k_diff <- lag(Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)
Panel$k_4diff <- Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003. - 
  lag(lag(lag(Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)))
for (i in 4:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-3]) {
    Panel$k_4diff[i] <- NA 
  }
}

#Panel$l_diff <- lag(Panel$Wage.earners.by.months..total)
Panel$l_4diff <- Panel$Wage.earners.by.months..total - lag(lag(lag(Panel$Wage.earners.by.months..total)))

for (i in 4:nrow(Panel)){
  if (Panel$ID.code[i]!=Panel$ID.code[i-3]) {
    Panel$l_4diff[i] <- NA 
  }
}

#Post <- Panel$Post_1929
#var_interest <- Panel$bank_sus_norm
#this regression is basically OLS, so I'll include an intercept
fourth_diff <- lm(Panel$y_4diff ~ Post*var_interest + Panel$k_4diff + Panel$l_4diff , data=Panel)
summary(fourth_diff)

Panel_4diff <- data.frame(Panel$Year, Panel$ID.code, Panel$y_4diff, Panel$Total.value.of.products, Panel$l_4diff,Panel$Wage.earners.by.months..total,  Panel$k_4diff, 
                         Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)

########### probit ###################################################
probit_panel <- subset(Panel , Panel$Open.in.1929 == 1)
y_4diff <-probit_panel$Total.value.of.products -  lag(lag(lag(probit_panel$Total.value.of.products)))
for (i in 4:nrow(probit_panel)){
  if (probit_panel$ID.code[i]!=probit_panel$ID.code[i-3]) {
    probit_panel$y_4diff[i] <- NA 
  }
}

probit_panel$k_4diff <- probit_panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003. - 
  lag(lag(lag(probit_panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)))
for (i in 4:nrow(probit_panel)){
  if (probit_panel$ID.code[i]!=probit_panel$ID.code[i-3]) {
    probit_panel$k_4diff[i] <- NA 
  }
}

#Panel$l_diff <- lag(Panel$Wage.earners.by.months..total)
probit_panel$l_4diff <- probit_panel$Wage.earners.by.months..total - lag(lag(lag(probit_panel$Wage.earners.by.months..total)))

for (i in 4:nrow(probit_panel)){
  if (probit_panel$ID.code[i]!=probit_panel$ID.code[i-3]) {
    probit_panel$l_4diff[i] <- NA 
  }
}

open_35 <- probit_panel$Open.in.1935
val_added_probit  <- probit_panel$Total.value.of.products
var_interest_probit <- probit_panel$alt_iv

Year <- probit_panel$Year
county <- probit_panel$County

library(dplyr)
labor_probit <- probit_panel$Wage.earners.by.months..total
capital_probit <- probit_panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
Post_probit <- probit_panel$Post_1929

labor_diff_quintile <- ntile(probit_panel$l_4diff, 5) 
capital_diff_quintile <- ntile(probit_panel$k_4diff, 5)

#probit_panel$labor <- labor
#probit_panel$capital <- capital
#probit_panel$labor_diff_quintile <- labor_diff_quintile
#probit_panel$capital_diff_quintile <- capital_diff_quintile

labor_quintile <- ntile(labor, 5) 
capital_quintile <- ntile(capital, 5) 


Post_probit <- probit_panel$Post_1929
#probit <- glm(open_35 ~ Post*var_interest, family=binomial(link="probit"), data=probit_panel)
probit <- glm(open_35 ~ Post_probit*var_interest_probit + probit_panel$labor_diff_quintile + probit_panel$capital_diff_quintile, family=binomial(link="probit"), data=probit_panel)
probit_no_error <- glm(open_35 ~ Post_probit*var_interest_probit + labor_probit + capital_probit, family=binomial(link="probit"), data=probit_panel)
#logit <- glm(open_35 ~ Post*var_interest + probit_panel$l_4diff, family=binomial(link="logit"), data=probit_panel)
#probit_correction <- logistf(open_35 ~ Post*var_interest, data=probit_panel)
summary(probit_no_error)

#cluster probit
#vcov_year_probit <- cluster.vcov(probit_no_error, probit_panel$County) #adjust variance covariance matrix via clustering
#probit_robust <- coeftest(probit_no_error, vcov_year_probit)
probit_robust <- coeftest(probit_no_error, vcov=vcovHC(probit_no_error,type="HC0",cluster="County"))

summary(probit_no_error)

vcov_year_probit <- cluster.vcov(probit_no_error, probit_panel$County) #adjust variance covariance matrix via clustering
probit_robust <- coeftest(probit_no_error, vcov_year_probit)
#################main robustness check(s)#############################
library(sqldf)
#Panel_robust <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx", 1)
Panel_robust <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx", 1)
panel_elements <- Panel_robust[c('Year', 'ID.code')]
dup <- data.frame(duplicated(panel_elements))

new_panel = data.frame(Panel_robust, dup)
Panel_robust <- new_panel[new_panel$duplicated.panel_elements. == F,]
Panel[Panel==''] <- NA

Panel_33 <- Panel_robust[(Panel_robust$Year == '1933') ,]
Panel_33['bank_sus_33'] <- Panel_33$FDIC_BANKS_SUS
Panel_33 <-Panel_33[order(Panel_33$ID.code,Panel_33$Year),]
Panel_35 <- Panel_robust[(Panel_robust$Year == '1935'),]
Panel_35['bank_sus_35'] <- Panel_35$FDIC_BANKS_SUS
Panel_35 <-Panel_35[order(Panel_35$ID.code,Panel_35$Year),]

Panel_31 <- Panel_robust[(Panel_robust$Year == '1931') ,]
Panel_31['bank_sus_31'] <- Panel_31$FDIC_BANKS_SUS
Panel_31 <-Panel_31[order(Panel_31$ID.code,Panel_31$Year),]

Panel_29 <- Panel_robust[(Panel_robust$Year == '1929') ,]
Panel_29['bank_sus_29'] <- Panel_29$FDIC_BANKS_SUS
#Panel_29['banks'] <- Panel_29$FDIC_BANKS
Panel_29 <-Panel_29[order(Panel_29$ID.code,Panel_29$Year),]
Panel_29['capital'] <- Panel_29['Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.']
Panel_29['labor'] <- Panel_29['Wage.earners.by.months..total']
Panel_29['output'] <- Panel_29['Total.value.of.products']

Panel_29['ID1'] <- Panel_29['ID.code']
Panel_31['ID2'] <- Panel_31['ID.code']

merged_29_31 <- sqldf('Select a.bank_sus_31, a.ID2 ,b.labor, b.capital,
                    b.output, b.County
                    FROM Panel_31 AS a JOIN 
                     Panel_29 AS b ON (a.ID2 = b.ID1)')
robust_1 <- lm(merged_29_31$bank_sus_31 ~ merged_29_31$capital + merged_29_31$labor, data=merged_29_31)
robust_2 <- lm(merged_29_31$bank_sus_31 ~ merged_29_31$output, data=merged_29_31$banks_sus_31)
summary(robust_1)
summary(robust_2)

Panel_33['ID1'] <- Panel_33['ID.code']
Panel_35['ID2'] <- Panel_35['ID.code']

merged_29_33 <-sqldf('Select a.* ,b.bank_sus_33 
                    FROM merged_29_31 AS a left OUTER JOIN 
                     Panel_33 AS b ON (a.ID2 = b.ID1)')

merged_29_35 <- sqldf('Select a.* ,b.bank_sus_35
                    FROM merged_29_33 AS a left OUTER JOIN 
                     Panel_35 AS b ON (a.ID2 = b.ID2)')
#county <- merged_29_35$County

#merged_29_35$bank_sus_31[is.na(merged_29_35$bank_sus_31)] <- 0
#merged_29_35$bank_sus_31[is.na(merged_29_35$bank_sus_33)] <- 0
#merged_29_35$bank_sus_31[is.na(merged_29_35$bank_sus_33)] <- 0



merged_29_35['bank_sus_31_35'] <- merged_29_35$bank_sus_31 + merged_29_35$bank_sus_33 + merged_29_35$bank_sus_35
#robust_3 <- lm(merged_29_35$bank_sus_31_35 ~ merged_29_35$capital + merged_29_35$labor, data=merged_29_35)
robust_check2 <- lm(merged_29_35$bank_sus_31_35 ~ merged_29_35$output + merged_29_35$capital + merged_29_35$labor + factor(merged_29_35$County), data=merged_29_35)
#summary(robust_3)
summary(robust_check2)

robust_check2_cluster <- coeftest(robust_check2, vcov=vcovHC(robust_check2,type="HC0",cluster="County"))


#---aggregation of all observations after 1929, for simplicity############################################
Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx", 1)
#Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var_auto.xlsx", 1)
CPI <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xls")


panel_elements <- Panel[c('Year', 'ID.code')]
dup <- data.frame(duplicated(panel_elements))

new_panel = data.frame(Panel, dup)
Panel <- new_panel[new_panel$duplicated.panel_elements. == F,]
Panel[Panel==''] <- NA

Panel<-Panel[order(Panel$ID.code,Panel$Year),]
Panel_post <- subset(Panel , Panel$Year != '1929')
Panel_pre <-subset(Panel , Panel$Year == '1929')
Panel_pre['ID'] <- Panel_pre$ID.code
Panel_post['ID'] <- Panel_post$ID.code
agg_columns <- cbind(Panel_post$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003., Panel_post$Total.value.of.products,
                     Panel_post$Wage.earners.by.months..total, Panel_post$alt_iv)

#I think this is Kosher right? -- yea dont do this, must of the NA's include 1929 as NA, so doesnt even matter
#for purposes of the regression
#agg_columns[is.na(agg_columns) & agg_columns[]] <- 0

Panel_agg <- aggregate(agg_columns, by=list(ID=Panel_post$ID), FUN=sum)
library(plyr)
Panel_agg <- rename(Panel_agg, c("V1"="Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.", 
                                 "V2"="Total.value.of.products", "V3"="Wage.earners.by.months..total", 
                                 "V4"="alt_iv" ))

#Panel_agg <- subset(Panel_agg,!duplicated(Panel_agg$ID))##
library(sqldf)

#this is the problem -- created 3 copies
Panel_agg <- sqldf('Select DISTINCT a.*, b.bank_sus_norm ,b.County
                    FROM Panel_agg AS a JOIN 
                     Panel_post AS b ON (a.ID = b.ID)')
Panel_agg['Post'] <- rep(1,nrow(Panel_agg))
Panel_agg['Year_fixed'] <- rep('Post',nrow(Panel_agg))
Panel_pre['Post'] <- rep(0,nrow(Panel_pre))
Panel_pre['Year_fixed'] <- rep('Pre',nrow(Panel_pre))

Panel_2per <- rbind(Panel_pre[,c('ID',"Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.",
                                      "Total.value.of.products", "Wage.earners.by.months..total",
                                      'alt_iv',"bank_sus_norm", 'County','Post', 'Year_fixed')], Panel_agg) 
Panel_2per<-Panel_2per[order(Panel_2per$ID,Panel_2per$Post),]

#sign of post*var_interest makes sense -- not so without interaction effect
fixed_2per <- lm(Panel_2per$Total.value.of.products ~ Panel_2per$Post * Panel_2per$bank_sus_norm
                 +  Panel_2per$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
                 + Panel_2per$Wage.earners.by.months..total + factor(Panel_2per$County) + Panel_2per$Post, 
                 data = Panel_2per)
summary(fixed_2per)

Panel_2per$alt_iv <- as.numeric(Panel_2per$alt_iv)

fixed_2per_alt_iv <- lm(Panel_2per$Total.value.of.products ~ Panel_2per$Post * Panel_2per$alt_iv
                 +  Panel_2per$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
                 + Panel_2per$Wage.earners.by.months..total + factor(Panel_2per$County) -1, 
                 data = Panel_2per)

summary(fixed_2per_alt_iv)

library(sandwich)
library(lmtest)
library(multiwayvcov)
require(multiwayvcov)
require(lmtest)

vcov_year_pre <- cluster.vcov(fixed_2per_alt_iv, Panel_2per$County)
fixed_2per_alt_iv_robust <- coeftest(fixed_2per_alt_iv, vcov_year_pre)
#im giving up on getting vcov to work here. Results are almost the same for this method below

#ask Hornbeck if white's estimator, clustered by County is ok
fixed_2per_alt_iv_robust <- coeftest(fixed_2per_alt_iv, vcov=vcovHC(fixed_2per_alt_iv,type="HC0",cluster="County"))
#run t test on model, with clustering covar matrix





#######make tables####
#table1 <- stargazer(fixed, fixed_robust,title="Fixed Characteristc IV", align=TRUE)
table2 <-  stargazer(alt_iv_robust, title="Fixed Effects Model", align=TRUE)

table3 <- stargazer(probit_robust, title="Probit Model", align=TRUE)
table4 <- stargazer(fixed_2per_alt_iv_robust, title="Fixed Effects -- Two Period Model", align=TRUE)
#table4 <- stargazer(first_diff, fourth_diff, title="Differenced Models", align=TRUE)
table5 <- stargazer(robust_check_clustered, title="Robustness Check 1", align=TRUE)
#table5 <- stargazer(robust_1, robust_2, title="Robustness Check 1", align=TRUE)
table5 <- stargazer(robust_check2_cluster, title="Robustness Check 2", align=TRUE)

#-----
merged_33_35 <-sqldf('Select a.bank_sus_33, a.ID1 ,b.bank_sus_35, 
                      FROM Panel_33 AS a JOIN 
                     Panel_35 AS b ON (a.ID1 = b.ID2)')
#merged_33_35 <- merge(x=data.frame(Panel_33$bank_sus_33, Panel_33$ID.code),y=data.frame(Panel_35$bank_sus_35, Panel_35$ID.code),by = 'ID.code')

banks_sus <- Panel_33$bank_sus_norm + Panel_35$bank_sus_norm
Panel_33_35<-Panel_33_35[order(Panel_33_35$ID.code,Panel_33_35$Year),]
Panel_33


#------
#group_var <- Panel$ID.code
#group_by_var <- as.factor(ID)
#as.numeric(ID)
#require(dplyr)
#Panel %>%
  #group_by(ID = c(Panel$ID.code)) %>%
  #group_by_(.dots=c("ID.code")) %>%
  #group_by_(ID) %>%
  #mutate(first_diff_var_interest = var_interest - lag(var_interest))
#Panel %>% 
  #group_by(factor(ID)) %>% 
  #mutate(first_diff_val_added = val_added - lag(val_added))
#Panel %>% 
 # group_by(factor(ID)) %>% 
 # mutate(first_diff_labor = labor - lag(labor))
#Panel %>% 
#  group_by(factor(ID)) %>%
  #mutate(first_diff_capital = capital - lag(capital))




#------
#test autocorrelation in variables -- from http://stats.stackexchange.com/questions/14914/how-to-test-the-autocorrelation-of-the-residuals
#mod = lm(val_added ~ fixed_capital)
#res = mod$res 
#n = length(res) 
#mod2 = lm(res[-n] ~ res[-1]) 
#summary(mod2)
# autocorrelation in residuals definitely seems to be happening....

#test serial correlation
pbgtest(fixed) #oh crap yea there's serial correlation

#cluster standard errors on county level--taken from https://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/

#get X matrix/predictors
X <- model.matrix(fixed)
# number of obs
n <- dim(X)[1]
# n of predictors
k <- dim(X)[2]
# calculate stan errs as in the above
# sq root of diag elements in vcov
#se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(fixed))/(n-k))))


# cluster name
cluster <- "County"
#Panel$County <- as.factor(Panel$County)
clus <- cbind(X,Panel[,cluster],resid(fixed))
colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
# number of clusters
m <- dim(table(clus[,cluster]))
dfc <- (m/(m-1))*((n-1)/(n-k))
# uj matrix
uclust <- matrix(NA, nrow = m, ncol = k) #took out the NA, left it blank
gs <- names(table(Panel[,cluster]))
#set i =1 ; convert county to factor then run it
for(i in 1:m){
  uclust[i,] <- t(matrix(clus[clus[,cluster]==gs[i],k+2])) %*% clus[clus[,cluster]==gs[i],1:k] 
}
se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc

###########Plotting -- maybe you find a functional form for production? #####
#ggplot(Panel, aes(x=labor, y=val_added)) + geom_point()
ggplot(Panel, aes(x=labor, y=val_added)) + geom_smooth(method='lm') + geom_point()
#ggplot(Panel, aes(x=capital, y=val_added)) + geom_point()
ggplot(Panel, aes(x=capital, y=val_added))  + geom_smooth(method='lm') + geom_point()
ggplot(Panel, aes(x=var_interest, y=val_added))  + geom_smooth(method='lm') + geom_point()
ggplot(Panel, aes(x=var_interest, y=labor))  + geom_smooth(method='lm') + geom_point()
#hist(var_interest, val_added)

#estimate ces production function using micro econ package
cesData <- data.frame(l = labor, k = capital)
# are these parametric assumption reasonable?
cesData$y2 <- cesCalc( xNames = c( "l", "k" ), data = cesData, coef = c( gamma = 1, delta = 0.6, rho = 0.5, nu = 1.1 ) )
Panel$y2 <- cesData$y2

ggplot(Panel, aes(x=val_added, y=y2)) + geom_point()
test = lm(val_added ~ y2 -1, data = Panel)
summary(test)

######for rough draft maybe? -- Maybe a better motivation is to plot against bank suspensions, not the iv
ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Wage.earners.by.months..total))   + geom_point() + stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Labor") + ggtitle("Scatterplot of Labor vs. Bank Distress (with OLS fit line)")
ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Capital") + ggtitle("Scatterplot of Capital vs. Bank Distress (with OLS fit line)")
ggplot(data = Panel, aes(x=Panel$alt_iv, y=Panel$Total.value.of.products))   + geom_point() +  stat_smooth(method = "lm", col = "red") + xlab("Bank Distress") + ylab("Output") + ggtitle("Scatterplot of Output vs. Bank Distress (with OLS fit line)")
