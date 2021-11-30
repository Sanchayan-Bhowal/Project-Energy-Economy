# Reproduction of page 7,8 in data paper
data <- read.csv("Data.csv")
library(timeDate)
library(DescTools)
describe <- function(variable) {
    z <- data.frame(
        Mean = mean(variable),
        Median = median(variable),
        Std = sd(variable),
        Skewness = skewness(variable),
        Kurtosis = kurtosis(variable),
        Smallest = min(variable),
        Largest = max(variable),
        Obs = length(variable),
        Normality = as.numeric(ShapiroFranciaTest(variable)[2])
    )
    return(z)
}

result <- rbind(
    describe(data$price_1),
    describe(data$price_2),
    describe(data$ln_price_1),
    describe(data$ln_price_2),
    describe(data$perc_change_p2_to_p1),
    describe(data$days_between_sale)
)

print(result)


windows()
par(mfrow = c(2, 2))
hist(data$ln_price_1,
    freq = F, breaks = 30, xlim = range(c(8, 16)),
    main = "Natural logarithm of first transaction price"
)
hist(data$ln_price_2,
    freq = F, breaks = 30, xlim = range(c(10, 16)),
    main = "Natural logarithm of second transaction price"
)
hist(data$perc_change_p2_to_p1,
    freq = F, breaks = 30,
    main = "Percentual price change between transactions"
)
hist(data$days_between_sale,
    freq = F, breaks = 35,
    main = "Period of time between both transactions"
)

epc <- function(x) {
    f <- as.numeric(table(x)[2])
    if (is.na(f)) {
        f <- 0
    }
    z <- data.frame(
        Frequency = f,
        Fraction = f / 4201
    )
    return(z)
}

epc_band <- rbind(
    epc(data$epc_rating_a),
    epc(data$epc_rating_b),
    epc(data$epc_rating_c),
    epc(data$epc_rating_d),
    epc(data$epc_rating_e),
    epc(data$epc_rating_f),
    epc(data$epc_rating_g)
)

print(epc_band)

windows()
par(mfrow = c(1, 2))
hist(data$epc_100,
    freq = F,
    main = "EPC standard assessment proceducre(SAP) points"
)
boxplot(data$epc_100,
    main = "EPC standard assessment proceducre(SAP) points"
)

windows()
par(mfrow = c(3, 2))

hist(data$imd_score,
    freq = F, xlab = "imd_score",
    main = "Index of Multiple deprivation(IMD) rank"
)
barplot(table(data$imd_level) / 4201,
    xlab = "imd_level",
    main = "Index of Multiple deprivation(IMD) decile"
)

hist(data$barrier_score,
    freq = F, xlab = "barrier_score",
    main = "Barriers to housing and service rank"
)
barplot(table(data$barrier_level) / 4201,
    xlab = "barrier_level",
    main = "Barriers to housing and service decile"
)

hist(data$crime_score,
    freq = F, xlab = "crime_score",
    main = "Crime rank"
)
barplot(table(data$crime_level) / 4201,
    xlab = "crime_level",
    main = "Crime decile"
)

windows()
par(mfrow = c(3, 2))

hist(data$educ_score,
    freq = F, xlab = "educ_score",
    main = "Education skills and training deprivation rank"
)
barplot(table(data$educ_level) / 4201,
    xlab = "educ_level",
    main = "Education skills and training deprivation decile"
)

hist(data$emp_score,
    freq = F, xlab = "emp_score",
    main = "Employment deprivation rank"
)
barplot(table(data$emp_level) / 4201,
    xlab = "emp_level",
    main = "Employment deprivation decile"
)

hist(data$health_score,
    freq = F, xlab = "health_score",
    main = "Health deprivation and disability rank"
)
barplot(table(data$health_level) / 4201,
    xlab = "health_level",
    main = "Health deprivation and disability level"
)

windows()
par(mfrow = c(2, 2))
hist(data$income_score,
    freq = F, xlab = "income_score",
    main = "Income deprivation rank"
)
barplot(table(data$income_level) / 4201,
    xlab = "income_level",
    main = "Income deprivation decile"
)

hist(data$living_score,
    freq = F, xlab = "living_score",
    main = "Living environment deprivation rank"
)
barplot(table(data$living_level) / 4201,
    xlab = "living_level",
    main = "Living environment deprivation decile"
)

geo <- function(x) {
    f <- as.numeric(table(x)[2])
    if (is.na(f)) {
        f <- 0
    }
    z <- data.frame(
        Transaction_Frequency = f,
        Transaction_Fraction = f / 4201
    )
    return(z)
}
geo_band <- rbind(
    geo(data$reg_north_west),
    geo(data$reg_yorkshire_and_the_humber),
    geo(data$reg_east_midlands),
    geo(data$reg_west_midlands),
    geo(data$reg_east_of_england),
    geo(data$reg_london),
    geo(data$reg_south_east),
    geo(data$reg_south_west),
    geo(data$reg_north_east)
)

print(geo_band)
# what is population fraction column in table 4 pg 10??