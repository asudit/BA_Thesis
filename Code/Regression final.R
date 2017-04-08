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

panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data final/regression_data edits ok.xlsx", 1)

is.na(panel) <- sapply(panel, is.infinite)
panel[mapply(is.infinite, panel)] <- NA
# remove infintie and NAn
#panel <- panel[is.finite(rowSums(panel)),]
#panel <- panel[!rowSums(!is.finite(panel)),]

y <- as.numeric(panel$Value.of.product)
y[!is.na(y) & y > 0] <- log(y[!is.na(y) & y > 0])
varying_iv <- as.numeric(panel$varying_iv)

firm <- as.factor(panel$firm.code)
year <- as.factor(panel$Year)

varying_iv <- lm(y ~ varying_iv + firm + year -1, data = panel)
