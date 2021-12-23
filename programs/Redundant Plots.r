Data <- read.csv("Data.csv") # nolint

# INDEX
# Hedonic regression of prices (band)
# relation of rating and time between sale
# hedonic regression of time
# relation of regions and house prices
# relation of regions and time between sale
# Hedonic regression of prices (continuous)
# Repeat sales index


# relation of rating and house prices
epc_rating <- Data$epc_rating_a +
    2 * Data$epc_rating_b +
    3 * Data$epc_rating_c +
    4 * Data$epc_rating_d +
    5 * Data$epc_rating_e + 6 * Data$epc_rating_f + 7 * Data$epc_rating_g
# epc_rating corresponds epc_rating, 1 for A, 2 for B ... 7 for G.

windows()
barplot(
    cbind(c(0, 0), rbind(
        tapply(Data$ln_price_1, epc_rating, mean),
        tapply(Data$ln_price_2, epc_rating, mean)
    )),
    beside = TRUE, ylim = c(0, 12),
    names.arg = c("A", "B", "C", "D", "E", "F", "G"),
    col = c("#00CEF6", "#3C78D8"),
    xlab = "epc_rating",
    ylab = "Mean of logarithm of price",
    main = "Relating of EPC with sales price",
)
legend("topleft", c("log price 1", "log price 2"),
    fill = c("#00CEF6", "#3C78D8"),
    bty = "n"
)

windows()
par(mfrow = c(1, 2))

boxplot(Data$ln_price_1 ~ epc_rating, ylab = "ln_price_1", col = c("#00CEF6"))
boxplot(Data$ln_price_2 ~ epc_rating, ylab = "ln_price_2", col = c("#3C78D8"))


# hedonic regression of prices
epc_price1 <- lm(ln_price_1 ~ epc_rating_b +
    epc_rating_c + epc_rating_e +
    epc_rating_f + epc_rating_g +
    reg_north_east +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)


epc_price2 <- lm(ln_price_2 ~ epc_rating_b +
    epc_rating_c + epc_rating_e +
    epc_rating_f + epc_rating_g +
    reg_north_east +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)


options(scipen = 999)
sink("OLS(band).txt")
print(summary(epc_price1))
print(summary(epc_price2))
# Summary shows the statistically significant variables of of ln_price
# Positivity of coefficients imply positive associativity


hedonic_price1 <- exp(epc_price1$coefficients) # hedonic price index1
hedonic_price2 <- exp(epc_price2$coefficients) # hedonic price index2

# confidence intervals for model coefficients
confint(epc_price1, conf.level = 0.95)
confint(epc_price2, conf.level = 0.95)


windows()
plot(1:13, hedonic_price1[-1],
    type = "n",
    axes = F,
    ylab = "",
    xlab = "",
    ylim = c(0.7, 2.1)
)

lines(1:13, hedonic_price1[-1], type = "o", col = "#3C78D8")
lines(1:13, hedonic_price2[-1], type = "o", col = "#8EC400")
axis(2)
axis(1,
    at = 1:13,
    labels = gsub("_", "\n", names(hedonic_price1)[-1]),
    padj = 1, pos = 0.7, cex.axis = 0.65
)
title(
    main = "Coefficients in OLS(band) estimations",
    ylab = "Coefficients",
    xlab = "Variables"
)

# Diagnostic plots for the model- 4 Residual plots-
# Residual = True value - Fitted value
# Residuals vs Fitted
# Normal Probability Plot of Residuals
# sqrt(|standardized residual|) vs fitted values
# Residuals vs Leverage
for (i in 1:6) {
    windows()
    par(mfrow = c(1, 2))
    plot(epc_price1, which = c(i), col = blues9)
    plot(epc_price2, which = c(i), col = blues9)
}

# Partial Residue plots
windows()
par(mfrow = c(4, 4))
termplot(epc_price1,
    partial.resid = TRUE, col.res = blues9, smooth = panel.smooth
)

windows()
par(mfrow = c(4, 4))
termplot(epc_price2,
    partial.resid = TRUE, col.res = blues9, smooth = panel.smooth
)


# relation of rating and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, epc_rating, mean),
    beside = TRUE,
    names.arg = c("B", "C", "D", "E", "F", "G"),
    col = c("#00CEF6", "#3C78D8"),
    xlab = "EPC rating", ylab = "Mean days between sale"
)
boxplot(Data$days_between_sale ~ epc_rating,
    xlab = "EPC rating", ylab = "Days between sale", col = c("#00CEF6")
)

sink()

# relation of region and rating
windows()
barplot(
    t(rbind(
        tapply(Data$reg_north_east, epc_rating, sum),
        tapply(Data$reg_north_west, epc_rating, sum),
        tapply(Data$reg_yorkshire_and_the_humber, epc_rating, sum),
        tapply(Data$reg_east_midlands, epc_rating, sum),
        tapply(Data$reg_west_midlands, epc_rating, sum),
        tapply(Data$reg_east_of_england, epc_rating, sum),
        tapply(Data$reg_london, epc_rating, sum),
        tapply(Data$reg_south_east, epc_rating, sum),
        tapply(Data$reg_south_west, epc_rating, sum)
    ) / 4201),
    ylim = c(0, 0.2),
    names.arg = gsub("_", " ", substring(colnames(Data)[36:44], 5)),
    col = rev(blues9),
    xlab = "Regions",
    ylab = "Frequency"
)

regions <- Data$reg_north_east +
    2 * Data$reg_north_west +
    3 * Data$reg_yorkshire_and_the_humber +
    4 * Data$reg_east_midlands +
    5 * Data$reg_west_midlands +
    6 * Data$reg_east_of_england +
    7 * Data$reg_london +
    8 * Data$reg_south_east +
    9 * Data$reg_south_west

# relation of regions and house prices
windows()
barplot(
    rbind(
        tapply(Data$ln_price_1, regions, mean),
        tapply(Data$ln_price_2, regions, mean)
    ),
    beside = TRUE, ylim = c(0, 14),
    names.arg = gsub("_", " ", substring(colnames(Data)[36:44], 5)),
    xlab = "Regions",
    col = c("#00CEF6", "#3C78D8"),
    ylab = "Mean of logarithm of sales price"
)

windows()
par(mfrow = c(1, 2))
boxplot(Data$ln_price_1 ~ regions,
    xlab = "Regions", ylab = "Logarithm of first sales price",
    col = c("#00CEF6")
)
boxplot(Data$ln_price_2 ~ regions,
    xlab = "Regions", ylab = "Logarithm of second sales price",
    col = c("#3C78D8")
)

# relation of regions and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, regions, mean),
    beside = TRUE,
    names.arg = gsub("_", " ", substring(colnames(Data)[36:44], 5)),
    xlab = "Regions",
    col = c("#00CEF6", "#3C78D8"),
    cex.axis = 0.65,
    ylab = "Mean of days between sale"
)

boxplot(Data$days_between_sale ~ regions,
    xlab = "Regions", ylab = "Mean of days between sale",
    col = c("#00CEF6")
)


# Continuous OLS
library(MASS)
windows()

price1_bc <- boxcox(price_1 ~ imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score, data = Data)

epc_price1_cont <- lm(ln_price_1 ~ imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score, data = Data)

price2_bc <- boxcox(price_2 ~ imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score, data = Data)

epc_price2_cont <- lm(t_price_2 ~ imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score, data = Data)

sink("OLS(continuous).txt")
print(summary(epc_price1_cont))
print(summary(epc_price2_cont))

hedonic_price1_cont <- epc_price1_cont$coefficients
hedonic_price2_cont <- epc_price2_cont$coefficients

# confidence intervals for model coefficients
confint(epc_price1_cont, conf.level = 0.95)
confint(epc_price2_cont, conf.level = 0.95)

windows()
plot(1:8, hedonic_price1_cont[-1],
    type = "n",
    axes = F,
    ylab = "",
    xlab = "",
)
lines(1:8, hedonic_price1_cont[-1], type = "o", col = "#3C78D8")
lines(1:8, hedonic_price2_cont[-1], type = "o", col = "#8EC400")
axis(2)
axis(1,
    at = 1:8,
    labels = gsub("_", "\n", names(hedonic_price1_cont)[-1]),
    padj = 1, pos = -0.00007
)
title(
    main = "Coefficients in OLS(continuous) estimations",
    ylab = "Coefficients",
    xlab = "Variables"
)
sink()

library(visreg)
windows()
par(mfrow = c(2, 4))
visreg(epc_price1_cont,
    trans = exp, ylab = "Price 1", ylim = c(0, 300000), partial = TRUE
)

transp2 <- function(x) {
    return((1 - 0.3030303 * x)^
        (1 / -0.3030303))
}
windows()
par(mfrow = c(2, 4))
visreg(epc_price2_cont,
    trans = transp2, ylab = "Price 2", ylim = c(0, 300000), partial = TRUE
)

# Repeat sales index
rsi <- read.csv("RSI.csv")
index <- lm(log_change ~ 0 + Y1996 + Y1997 + Y1998 + Y1999 +
    Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008
    + Y2009 + Y2010 + Y2011 + Y2012, data = rsi)
windows()
rs_index <- exp(coef(index))
plot(rs_index, type = "o", axes = F, xlab = "", ylab = "", col = "#3C78D8")
axis(2)
axis(1,
    at = 1:17,
    labels = gsub("Y", "", colnames(rsi)[4:20]),
    padj = 1
)
title(
    main = "Repeated Sales Index",
    ylab = "Index",
    xlab = "Year"
)