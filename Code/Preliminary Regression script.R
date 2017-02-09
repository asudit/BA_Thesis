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
#Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx", 1)
Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx", 1)
CPI <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/CPI Unadjusted,annual,index units.xls")


panel_elements <- Panel[c('Year', 'ID.code')]
dup <- data.frame(duplicated(panel_elements))

new_panel = data.frame(Panel, dup)
Panel <- new_panel[new_panel$duplicated.panel_elements. == F,]
#Panel1 <- group_by(Panel, 'Id.code')

#Panel <- lapply(Panel, function(x) x[is.finite(x)])

#for(i in 1:ncol(Panel)){
  #aa <- sapply(Panel[[i]], function(x){return(is.finite(x))})
#}
#Panel$noInf <- aa
#Panel <- Panel[Panel['noInf'] == T,]


var_interest <- Panel$bank_sus_norm
var_interest <- as.numeric(var_interest)

alt_iv = as.numeric(Panel$alt_iv)

val_added  <- Panel$Total.value.of.products
ID <- Panel$ID.code
Year <- Panel$Year
county <- Panel$County
subsidiary <- Panel$Branch.or.subsidiary.of.other.firm

labor <- Panel$Wage.earners.by.months..total
#fixed_capital <- Panel$Total.HP
#variable_capital <- Panel$Cost.of.all.materials.and.raw.stock.actually.used
capital <- Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
#Nanda gets the credit
Post <- Panel$Post_1929


#issue -- the merge with FDIC did not work as well as hoped. lots of blanks

# this using built in fixed effects
#fixed <- plm(val_added ~  Post*var_interest  +  labor + capital, data=Panel, index=c('County', 'Year'), model="within")

#this is using my own fixed effects
#county <-as.factor(county)
#Year <- as.factor(Year)
fixed <- lm(val_added ~ Post*var_interest + labor + capital + factor(county) + factor(Year) -1 , data=Panel)
#fixed_alt_iv <- lm(val_added ~ Post*alt_iv + labor + capital + factor(county) + factor(Year) -1 , data=Panel)

summary(fixed)
#summary(fixed_alt_iv)
labor_elasticity <- lm(labor ~ var_interest)
summary(labor_elasticity)
#cap_elasticity <- lm(capital ~ var_interest)
#summary(cap_elasticity)
#random <- plm(val_added ~ banks_sus_ratio + labor + capital, data=Panel, index=c('ID.code', 'Year'), model="random")

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

first_diff <- lm(Panel$y_diff ~ Post*var_interest + Panel$k_diff + Panel$l_diff + factor(county) + factor(Year) -1 , data=Panel)
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

fourth_diff <- lm(Panel$y_4diff ~ Post*var_interest + Panel$k_4diff + Panel$l_4diff -1 , data=Panel)
summary(fourth_diff)

Panel_4diff <- data.frame(Panel$Year, Panel$ID.code, Panel$y_4diff, Panel$Total.value.of.products, Panel$l_4diff,Panel$Wage.earners.by.months..total,  Panel$k_4diff, 
                         Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.)

########### robustness check ##############






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

#################main robustness check(s)######



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

#estimate ces production function using micro econ package
cesData <- data.frame(l = labor, k = capital)
# are these parametric assumption reasonable?
cesData$y2 <- cesCalc( xNames = c( "l", "k" ), data = cesData, coef = c( gamma = 1, delta = 0.6, rho = 0.5, nu = 1.1 ) )
Panel$y2 <- cesData$y2

ggplot(Panel, aes(x=val_added, y=y2)) + geom_point()
test = lm(val_added ~ y2 -1, data = Panel)
summary(test)


