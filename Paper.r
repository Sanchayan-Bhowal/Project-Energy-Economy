Data <- read.csv("Data.csv") # nolint

#INDEX
#Hedonic regression of prices (band)
#relation of rating and time between sale
#hedonic regression of time
#relation of regions and house prices
#relation of regions and time between sale
#Hedonic regression of prices (continuous)
#Repeat sales index


# relation of rating and house prices
epc_rating <- Data$epc_rating_a +
    2 * Data$epc_rating_b +
    3 * Data$epc_rating_c +
    4 * Data$epc_rating_d +
    5 * Data$epc_rating_e + 6 * Data$epc_rating_f + 7 * Data$epc_rating_g
# epc_rating corresponds epc_rating, 1 for A, 2 for B ... 7 for G.

windows()
barplot(
    rbind(
        tapply(Data$ln_price_1, epc_rating, mean),
        tapply(Data$ln_price_2, epc_rating, mean)
    ),
    beside = TRUE, ylim = c(0, 12),
    names.arg = c("B", "C", "D", "E", "F", "G"),
    xlab = "epc_rating",
    ylab = "Mean of logarithm of price",
    main = "Relating of EPC with sales price",
)
windows()
par(mfrow = c(1, 2))

boxplot(Data$ln_price_1 ~ epc_rating, ylab = "ln_price_1")
boxplot(Data$ln_price_2 ~ epc_rating, ylab = "ln_price_2")


# hedonic regression of prices
epc_price1 <- lm(ln_price_1 ~ epc_rating_a + epc_rating_b +
    epc_rating_c + epc_rating_d +
    epc_rating_e + epc_rating_f + epc_rating_g +
    imd_level + income_level +
    emp_level + educ_level +
    health_level + crime_level +
    barrier_level + living_level +
    reg_north_east + reg_north_west +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)


epc_price2 <- lm(ln_price_2 ~ epc_rating_a + epc_rating_b +
    epc_rating_c + epc_rating_d +
    epc_rating_e + epc_rating_f + epc_rating_g +
    imd_level + income_level +
    emp_level + educ_level +
    health_level + crime_level +
    barrier_level + living_level +
    reg_north_east + reg_north_west +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)


options(scipen = 999)
sink("OLS(band).txt")
print(summary(epc_price1))
print(summary(epc_price2))
# Summary shows the statistically significant variables of of ln_price
# Positivity of coefficients imply positive associativity

epc_price1$coefficients[is.na(epc_price1$coefficients)] <- 0
epc_price2$coefficients[is.na(epc_price2$coefficients)] <- 0

hedonic_price1 <- exp(epc_price1$coefficients) # hedonic price index1
hedonic_price2 <- exp(epc_price2$coefficients) # hedonic price index2

#confidence intervals for model coefficients
confint(epc_price1, conf.level=0.95)
confint(epc_price2, conf.level=0.95)


windows()
plot(1:24, hedonic_price1[-1],
    type = "n",
    axes = F,
    ylab = "",
    xlab = "",
    ylim = c(0.7, 1.9)
)

lines(1:24, hedonic_price1[-1], type = "o", col = "blue")
lines(1:24, hedonic_price2[-1], type = "o", col = "green")
axis(2)
axis(1,
    at = 1:24,
    labels = gsub("_", "\n", names(hedonic_price1)[-1]),
    padj = 1, pos = 0.7, cex.axis = 0.65
)
title(
    main = "Coefficients in OLS(continuous) estimations",
    ylab = "Coefficients",
    xlab = "Variables"
)

# diagnostic plots for the model- 4 Residual plots- Residual = True value - Fitted value
# Residuals vs Fitted
# Normal Probability Plot of Residuals
# sqrt(|standardized residual|) vs fitted values
# Residuals vs Leverage
windows()
par(mfrow = c(2, 2))
plot(epc_price1)
windows()
par(mfrow = c(2, 2))
plot(epc_price2)

# Partial Residue plots
windows()
par(mfrow = c(4, 6))
termplot(epc_price1,
    partial.resid = TRUE, col.res = "purple", smooth = panel.smooth
)

windows()
par(mfrow = c(4, 6))
termplot(epc_price2,
    partial.resid = TRUE, col.res = "purple", smooth = panel.smooth
)


# relation of rating and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, epc_rating, mean),
    beside = TRUE,
    names.arg = c("B", "C", "D", "E", "F", "G"),
    xlab = "EPC rating", ylab = "Mean days between sale"
)
boxplot(Data$days_between_sale ~ epc_rating,
    xlab = "EPC rating", ylab = "Days between sale"
)

# hedonic regression of time
epc_time <- lm(days_between_sale ~ epc_rating_a + epc_rating_b +
    epc_rating_c + epc_rating_d +
    epc_rating_e + epc_rating_f + epc_rating_g +
    imd_level + income_level +
    emp_level + educ_level +
    health_level + crime_level +
    barrier_level + living_level +
    reg_north_east + reg_north_west +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)
print(summary(epc_time))
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
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5)),
    xlab = "Regions",
    ylab = "Sum of EPC rating"
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
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5)),
    xlab = "Regions",
    ylab = "Mean of logarithm of sales price"
)

windows()
par(mfrow = c(1, 2))
boxplot(Data$ln_price_1 ~ regions,
    xlab = "Regions", ylab = "Logarithm of first sales price"
)
boxplot(Data$ln_price_2 ~ regions,
    xlab = "Regions", ylab = "Logarithm of second sales price"
)

# relation of regions and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, regions, mean),
    beside = TRUE,
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5)),
    xlab = "Regions",
    ylab = "Mean of days between sale"
)

boxplot(Data$days_between_sale ~ regions,
    xlab = "Regions", ylab = "Mean of days between sale"
)

# Continuous OLS
epc_price1_cont <- lm(ln_price_1 ~ ln_epc_100 +
    imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score +
    reg_north_east + reg_north_west +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)


sink("OLS(continuous).txt")
print(summary(epc_price1_cont))

epc_price2_cont <- lm(ln_price_2 ~ ln_epc_100 +
    imd_score + income_score +
    emp_score + educ_score +
    health_score + crime_score +
    barrier_score + living_score +
    reg_north_east + reg_north_west +
    reg_yorkshire_and_the_humber + reg_east_midlands +
    reg_west_midlands + reg_east_of_england +
    reg_london + reg_south_east + reg_south_west, data = Data)

print(summary(epc_price2_cont))

epc_price1_cont$coefficients[is.na(epc_price1_cont$coefficients)] <- 0
epc_price2_cont$coefficients[is.na(epc_price2_cont$coefficients)] <- 0

hedonic_price1_cont <- exp(epc_price1_cont$coefficients)
hedonic_price2_cont <- exp(epc_price2_cont$coefficients)

#confidence intervals for model coefficients
confint(epc_price1_cont, conf.level=0.95)
confint(epc_price2_cont, conf.level=0.95)

windows()
plot(1:18, hedonic_price1_cont[-1],
    type = "n",
    axes = F,
    ylab = "",
    xlab = "",
    ylim = c(0.5, 1.65)
)
lines(1:18, hedonic_price1_cont[-1], type = "o", col = "blue")
lines(1:18, hedonic_price2_cont[-1], type = "o", col = "green")
axis(2)
axis(1,
    at = 1:18,
    labels = gsub("_", "\n", names(hedonic_price1_cont)[-1]),
    padj = 1, pos = 0.6
)
title(
    main = "Coefficients in OLS(continuous) estimations",
    ylab = "Coefficients",
    xlab = "Variables"
)
sink()

windows()
par(mfrow = c(4, 5))
termplot(epc_price1_cont,
    partial.resid = TRUE, col.res = "purple", smooth = panel.smooth
)

windows()
par(mfrow = c(4, 5))
termplot(epc_price2_cont,
    partial.resid = TRUE, col.res = "purple", smooth = panel.smooth
)

# Repeat sales index
rsi <- read.csv("RSI.csv")
index <- lm(log_change ~ 0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 +
    Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008
    + Y2009 + Y2010 + Y2011 + Y2012, data = rsi)
windows()
rs_index <- exp(coef(index))
rs_index[is.na(rs_index)] <- 1
plot(rs_index, type = "o", axes = F, xlab = "", ylab = "", col = "blue")
axis(2)
axis(1,
    at = 1:18,
    labels = gsub("Y", "", colnames(rsi)[3:20]),
    padj = 1
)
title(
    main = "Repeated Sales Index",
    ylab = "Index",
    xlab = "Year"
)