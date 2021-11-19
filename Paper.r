data <- read.csv("data.csv")

# relation of rating and house prices
epc_rating <- data$epc_rating_a +
    2 * data$epc_rating_b +
    3 * data$epc_rating_c +
    4 * data$epc_rating_d +
    5 * data$epc_rating_e + 6 * data$epc_rating_f + 7 * data$epc_rating_g

windows()
barplot(
    rbind(
        tapply(data$ln_price_1, epc_rating, mean),
        tapply(data$ln_price_2, epc_rating, mean)
    ),
    beside = TRUE, ylim = c(0, 12),
    names.arg = c("B", "C", "D", "E", "F", "G")
)

windows()
par(mfrow = c(1, 2))
boxplot(data$ln_price_1 ~ epc_rating)
boxplot(data$ln_price_2 ~ epc_rating)

# hedonic regression of prices wrt rating
epc_price1 <- lm(ln_price_1 ~ epc_rating_a + epc_rating_b +
    epc_rating_c + epc_rating_d +
    epc_rating_e + epc_rating_f + epc_rating_g, data = data)

epc_price2 <- lm(ln_price_2 ~ epc_rating_a + epc_rating_b +
    epc_rating_c + epc_rating_d +
    epc_rating_e + epc_rating_f + epc_rating_g, data = data)

sink("linear models.txt")
print(summary(epc_price1))
print(summary(epc_price2))
sink()

# relation of rating and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(data$days_between_sale, epc_rating, mean),
    beside = TRUE,
    names.arg = c("B", "C", "D", "E", "F", "G")
)

boxplot(data$days_between_sale ~ epc_rating)


# relation of region and rating
windows()
barplot(
    t(rbind(
        tapply(data$reg_north_east, epc_rating, sum),
        tapply(data$reg_north_west, epc_rating, sum),
        tapply(data$reg_yorkshire_and_the_humber, epc_rating, sum),
        tapply(data$reg_east_midlands, epc_rating, sum),
        tapply(data$reg_west_midlands, epc_rating, sum),
        tapply(data$reg_east_of_england, epc_rating, sum),
        tapply(data$reg_london, epc_rating, sum),
        tapply(data$reg_south_east, epc_rating, sum),
        tapply(data$reg_south_west, epc_rating, sum)
    ) / 4201),
    ylim = c(0, 0.2),
    names.arg = gsub("_", " ", substring(colnames(data)[35:43], 5))
)

regions <- data$reg_north_east +
    2 * data$reg_north_west +
    3 * data$reg_yorkshire_and_the_humber +
    4 * data$reg_east_midlands +
    5 * data$reg_west_midlands +
    6 * data$reg_east_of_england +
    7 * data$reg_london +
    8 * data$reg_south_east +
    9 * data$reg_south_west

# relation of regions and house prices
windows()
barplot(
    rbind(
        tapply(data$ln_price_1, regions, mean),
        tapply(data$ln_price_2, regions, mean)
    ),
    beside = TRUE, ylim = c(0, 14),
    names.arg = gsub("_", " ", substring(colnames(data)[35:43], 5))
)

windows()
par(mfrow = c(1, 2))
boxplot(data$ln_price_1 ~ regions)
boxplot(data$ln_price_2 ~ regions)

# relation of regions and time between sale
windows()
par(mfrow = c(1, 2))
barplot(
    tapply(data$days_between_sale, regions, mean),
    beside = TRUE,
    names.arg = gsub("_", " ", substring(colnames(data)[35:43], 5))
)

boxplot(data$days_between_sale ~ regions)