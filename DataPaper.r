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

border <- "#092f4e"
fill <- "#204c71"

windows()
par(mfrow = c(2, 2))
ggplot(data = data, aes(ln_price_1)) + geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.18, col = border, fill = fill) + ylab("Fraction") + ggtitle("Natural logarithm of first transaction price")
hist(data$ln_price_1,
    freq = F, breaks = 30,
    xlim = range(c(8, 16)), col = "#3C78D8",
    main = "Natural logarithm of first transaction price"
)
ggplot(data = data, aes(ln_price_2)) + geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.18, col = border, fill = fill) + ylab("Fraction") + ggtitle("Natural logarithm of second transaction price")
hist(data$ln_price_2,
    freq = F, breaks = 30,
    xlim = range(c(10, 16)), col = "#3C78D8",
    main = "Natural logarithm of second transaction price"
)
ggplot(data = data, aes(perc_change_p2_to_p1)) + geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.3, col = border, fill = fill) + ylab("Fraction") + ggtitle("Percentual price change between transactions")
hist(data$perc_change_p2_to_p1,
    freq = F, breaks = 30, col = "#3C78D8",
    main = "Percentual price change between transactions"
)
ggplot(data = data, aes(days_between_sale)) + geom_histogram(aes(y = ..count../sum(..count..)), bins = 35, col = border, fill = fill) + ylab("Fraction") + ggtitle("Period of time between both transactions")
hist(data$days_between_sale,
    freq = F, breaks = 35, col = "#3C78D8",
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
    freq = F, col = "#3C78D8",
    main = "EPC standard assessment proceducre(SAP) points"
)
boxplot(data$epc_100,
    col = "#2FD7B4",
    main = "EPC standard assessment proceducre(SAP) points"
)

windows()
par(mfrow = c(3, 2))

hist(data$imd_score,
    freq = F, xlab = "imd_score", col = "#3C78D8",
    main = "Index of Multiple deprivation(IMD) rank"
)
barplot(table(data$imd_level) / 4201,
    xlab = "imd_level",
    col = blues9,
    main = "Index of Multiple deprivation(IMD) decile"
)

hist(data$barrier_score,
    freq = F, xlab = "barrier_score", col = "#3C78D8",
    main = "Barriers to housing and service rank"
)
barplot(table(data$barrier_level) / 4201,
    xlab = "barrier_level",
    col = blues9,
    main = "Barriers to housing and service decile"
)

hist(data$crime_score,
    freq = F, xlab = "crime_score", col = "#3C78D8",
    main = "Crime rank"
)
barplot(table(data$crime_level) / 4201,
    xlab = "crime_level",
    col = blues9,
    main = "Crime decile"
)

windows()
par(mfrow = c(3, 2))

hist(data$educ_score,
    freq = F, xlab = "educ_score", col = "#3C78D8",
    main = "Education skills and training deprivation rank"
)
barplot(table(data$educ_level) / 4201,
    xlab = "educ_level",
    col = blues9,
    main = "Education skills and training deprivation decile"
)

hist(data$emp_score,
    freq = F, xlab = "emp_score", col = "#3C78D8",
    main = "Employment deprivation rank"
)
barplot(table(data$emp_level) / 4201,
    xlab = "emp_level",
    col = blues9,
    main = "Employment deprivation decile"
)

hist(data$health_score,
    freq = F, xlab = "health_score", col = "#3C78D8",
    main = "Health deprivation and disability rank"
)
barplot(table(data$health_level) / 4201,
    xlab = "health_level",
    col = blues9,
    main = "Health deprivation and disability level"
)

windows()
par(mfrow = c(2, 2))
hist(data$income_score,
    freq = F, xlab = "income_score", col = "#3C78D8",
    main = "Income deprivation rank"
)
barplot(table(data$income_level) / 4201,
    xlab = "income_level",
    col = blues9,
    main = "Income deprivation decile"
)

hist(data$living_score,
    freq = F, xlab = "living_score", col = "#3C78D8",
    main = "Living environment deprivation rank"
)
barplot(table(data$living_level) / 4201,
    xlab = "living_level",
    col = blues9,
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