Data <- read.csv("Data.csv") # nolint

# relation of rating and house prices
epc_rating <- Data$epc_rating_a +
    2 * Data$epc_rating_b +
    3 * Data$epc_rating_c +
    4 * Data$epc_rating_d +
    5 * Data$epc_rating_e + 6 * Data$epc_rating_f + 7 * Data$epc_rating_g

windows()
barplot(
    rbind(
        tapply(Data$ln_price_1, epc_rating, mean),
        tapply(Data$ln_price_2, epc_rating, mean)
    ),
    beside = TRUE, ylim = c(0, 12),
    names.arg = c("B", "C", "D", "E", "F", "G")
)

windows()
par(mfrow = c(1, 2))
boxplot(Data$ln_price_1 ~ epc_rating)
boxplot(Data$ln_price_2 ~ epc_rating)

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

sink("linear models.txt")
print(summary(epc_price1))
print(summary(epc_price2))

epc_price1$coefficients[is.na(epc_price1$coefficients)] <- 0
epc_price2$coefficients[is.na(epc_price2$coefficients)] <- 0

hedonic_price1 <- exp(epc_price1$coefficients)
hedonic_price2 <- exp(epc_price2$coefficients)

windows()
out <- plot(1:24, hedonic_price1[-1], type = "o", col = "blue")
text(1:24,
    rep(0.7, 25),
    gsub("_", "\n", names(hedonic_price1)[-1]),
    srt = 90, pos = 1, cex = 0.65, xpd = NA
)
lines(1:24, hedonic_price2[-1], type = "o", col = "green")

# relation of rating and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, epc_rating, mean),
    beside = TRUE,
    names.arg = c("B", "C", "D", "E", "F", "G")
)

boxplot(Data$days_between_sale ~ epc_rating)

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
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5))
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
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5))
)

windows()
par(mfrow = c(1, 2))
boxplot(Data$ln_price_1 ~ regions)
boxplot(Data$ln_price_2 ~ regions)

# relation of regions and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(Data$days_between_sale, regions, mean),
    beside = TRUE,
    names.arg = gsub("_", " ", substring(colnames(Data)[35:43], 5))
)

boxplot(Data$days_between_sale ~ regions)