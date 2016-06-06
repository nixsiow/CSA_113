# ===== START OF SCRIPT =====
# SEB 113
# Collaborative Scientific Article (CSA)

# === Group member === #
# Last Name: Siow
# First Name: Yun Kai
# Student No.: 9598138
# Course code: SE50

# ===== Dependency check =====
## pacman, the package manager to check whether 
## dependency libraries are installed, and load it if it does
if (!require("pacman")) install.packages("pacman") # install pacman itself if it's not already
pacman::p_load(ggplot2, dplyr, openair, GGally, reshape2, broom, lubridate, ggmap) # pacman to install/load libraries
# ===== Dependency check =====

## read raw csv file from cwd
# air.quality.clinton.raw <- read.csv(file="clinton-aq-2015.csv", as.is=T, head=T)

# Or read directly from data source.
data.source.url <- "http://www.ehp.qld.gov.au/data-sets/air-quality/clinton-aq-2015.csv"
air.quality.clinton.raw <- read.csv(file=data.source.url, as.is=T, head=T)

## Have a look on the first few data entries in the raw data frame
head(air.quality.clinton.raw)
## Dimension of raw data frame
dim(air.quality.clinton.raw) # it has 8760 entries and 17 variables
## Quick way to summarize the raw data frame
summary(air.quality.clinton.raw)

## ==================== ## 
## DATA WRANGLING
## ==================== ## 
# Lubridate to deal with date and time. Convert them to single variable colume
# with POSIXct format so that R can understand it. 
# Overwrite the old "Date" variable.
air.quality.clinton.raw$Date <- dmy_hm(paste(air.quality.clinton.raw$Date, air.quality.clinton.raw$Time))
# get rid of the old "Time" variable colume by assigning NULL to it
# air.quality.clinton.raw$Time <- NULL
# Lubridate to add few extra colume: day, month, year, yday, day_of_week
air.quality.clinton.raw <- mutate(air.quality.clinton.raw,
                                  # day = day(Date),
                                  month = month(Date, label=T),
                                  # year = year(Date),
                                  # yday = yday(Date),
                                  day_of_week = wday(Date, label=T))

## define data of interest, rearrange the seq
data.of.interest <- c("Date", "Time", "month", "day_of_week", "PM2.5..ug.m.3.", "Wind.Speed..m.s.", "Wind.Direction..degTN.")
air.quality.clinton <- subset(air.quality.clinton.raw, select = data.of.interest)
## Rename variable name
names(air.quality.clinton) <- c("date", "time", "month", "day_of_week", "pm2.5", "ws", "wd")
head(air.quality.clinton)

## Assign breakboint for cutting and labelling
## 0 and 360 is for NORTH
breaks = c(0, seq(22.5, 337.5, by=45), 360)

## cut function dplyr to divides the range of feeded data into
## intervals and codes the values in "Direction" such as NE, E ...
## according to which interval they fall.
## Turn the continuous variable to categorial variable
wd.label <- cut(air.quality.clinton$wd, breaks = breaks, dig.lab= 4, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"), include.lowest = TRUE)

## Create new variable wd.label & log.pm2.5
air.quality.clinton <- mutate(air.quality.clinton, wd.label = wd.label) %>% 
  mutate(log.pm2.5 = log(pm2.5)) %>%
  na.omit %>%
  filter(!is.nan(log.pm2.5) & !is.infinite(log.pm2.5))

# Check the levels of wd.label
levels(air.quality.clinton$wd.label)
# regroup both "N" level into only one level, should be only 8 instead of 9
levels(air.quality.clinton$wd.label) <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")

## Check the range of produced log.pm2.5
range(air.quality.clinton$log.pm2.5)
## Check the latest data frame
head(air.quality.clinton)
##
summary(air.quality.clinton)


# # Use small multiples of scatter plots to show relationships between IDVs, DV
# # FIX IT, WONT WORK WITH CATEGORICAL VARIABLE
# air.vars <- c("wd.label", "ws", "log.pm2.5" )
# air.pairs <- 
#   ggpairs(air.quality.clinton, columns = air.vars,
#           columnLabels = c("Wind Direction, label", "Wind Speed", "Log PM 2.5"),
#           lower=list(continuous="smooth"), diag=NULL, title="Correlation between explanatory variables and outcome variable") +
#   theme_bw()
# air.pairs
# 
# # Another way to show correlation in between IDVs, DV. 
# # Correlation matrix of our data
# # FIX IT, WONT WORK WITH CATEGORICAL VARIABLE
# air.cor <- round(cor(air.quality.clinton[,air.vars]), 3)
# air.cor
# 
# # Further visualisation, heatmap of correlation
# # FIX IT, WONT WORK WITH CATEGORICAL VARIABLE
# cor.melt <- melt(air.cor)
# air.cor.heat <- ggplot(data=cor.melt, aes(x=Var1, y=Var2)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0, limits=c(-1,1)) +
#   theme(axis.text.x=element_text(angle=-90, hjust=0)) + coord_equal() + xlab("") + ylab("")
# air.cor.heat

## ==================== ## 
## Exploratory Data Visualisation
## ==================== ## 

# Plot the wind direction in pie chart
ggplot(data=air.quality.clinton, aes(x=wd.label, fill = factor(wd.label))) + 
  geom_bar(width = 1, color="white", alpha=0.7) + 
  # rotation of the pie chart to 5.89 radian, clockwise.
  coord_polar(start = 5.89, direction=1) +
  theme_bw() +
  theme(legend.position="none") +
  labs(fill="Wind\nDirection",
       title = "Frequency of various wind direction", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Wind Direction", 
       y = "Count")

# Plot the pm2.5 concentration varies according to wind direction in boxplot
p <- ggplot(data=air.quality.clinton, aes(x=wd.label, y=log.pm2.5)) + 
  geom_boxplot(outlier.colour = NULL, aes_string(colour="wd.label", fill="wd.label")) +
  # scale_y_log10() +
  theme_bw() +
  theme(legend.position="none") +
  labs(title = "PM2.5 concentration varies according to wind direction", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Wind Direction", 
       y = "PM 2.5")
# use the details of the plot, to derive the coordinates of where the median line is,
# and then add colour to it using geom_segment.
dat <- ggplot_build(p)$data[[1]]
p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                               y=middle, yend=middle), 
                 colour="white", size=0.8)


# Plot the pm2.5 concentration in air in histrogram
ggplot(data=air.quality.clinton, aes(x=pm2.5)) + 
  geom_histogram()

# Plot the pm2.5 concentration in air in histrogram with log scale
ggplot(data=air.quality.clinton, aes(x=log.pm2.5)) + 
  geom_histogram()

#
ggplot(data=air.quality.clinton, aes(x=ws, y=pm2.5)) +
  geom_point()


# Look up table for facet_wrap label
Direction <- c(
  N = "North",
  NE = "North East",
  E = "East",
  SE = "South East",
  S = "South",
  SW = "South West",
  W = "West",
  NW = "North West",
  N = "North"
)
# global labeller used for facet label
global_labeller <- labeller(
  wd.label = Direction
)

# Facet plot, log PM2.5 concentration varies according to wind speed and wind direction
ggplot(data=air.quality.clinton, aes(x=ws, y=log.pm2.5)) +
  geom_point(aes(color=wd.label), size=4, alpha=0.3) +
  geom_smooth(method="lm", se=FALSE, color="grey40", alpha=0.6, size=0.5) +
  facet_wrap(~wd.label, nrow = 2, 
             labeller = global_labeller) + 
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title = "PM2.5 concentration correlated to Wind speed", 
       subtitle = "further split it into 8 different wind direction facet", 
       x = expression(paste("Wind speed, ", ms^-1)), 
       y = expression(paste(log," ",PM[2.5]," ",mu,g,m^-3))
       )


## ==================== ## 
## Further visualisation::openair package
## ==================== ## 
# polar plot
polarPlot(air.quality.clinton, pollutant = 'pm2.5', resolution="fine")
#
polarPlot(air.quality.clinton, pollutant = 'pm2.5', type="time")
polarPlot(air.quality.clinton, pollutant = 'pm2.5', hemisphere = "southern", type="season")
polarPlot(air.quality.clinton, pollutant = 'pm2.5', type="weekend")
polarPlot(air.quality.clinton, pollutant = 'pm2.5', type=c("season", "weekday"))

polarPlot(air.quality.clinton,
          resolution="fine",
          pollutant="pm2.5",
          col="jet",
          key.position="bottom",
          key.header="mean PM2.5 (ug/m3)",
          key.footer=NULL,
          ylab = "Wind Speed (m/s)",
          main = "Polar plot of mean of PM2.5 concentration in all direction")


# Wind rose, showing how wind speed and wind direction conditions vary by year. 
windRose(air.quality.clinton)
windRose(air.quality.clinton, type="month")

# Pollution rose 
pollutionRose(air.quality.clinton, pollutant = 'pm2.5')
# e.g. 'hour', 'weekday', 'month', 'daylight' (to analyse data by daytime/nighttime).
pollutionRose(air.quality.clinton, pollutant = 'pm2.5', type = 'season')
pollutionRose(air.quality.clinton, pollutant = 'pm2.5', type = 'daylight')

# First, we choose to deseasonalise the data, 
# then we choose to plot percentiles (the 5th, 50th, 75th and 95th), 
# then we set type to 'wd'. 
smoothTrend(air.quality.clinton, pollutant = "pm2.5", deseason = TRUE, statistic = "percentile", percentile = c(25, 50, 75, 95), type = "wd")

# percentileRose
percentileRose(air.quality.clinton,pollutant="pm2.5", smooth=TRUE)

percentileRose(air.quality.clinton,pollutant="pm2.5",percentile=c(25,50,75,90,95,99,99.9),col="brewer1",key.position="right",smooth=TRUE)


# an arrow can be shown for each day giving the vector-averaged wind direction.
# the arrow can be scaled according to the wind speed to highlight both the direction and strength of the wind on a particular day,
# which can help show the influence of meteorology on pollutant concentrations
calendarPlot(air.quality.clinton, pollutant = "pm2.5")

calendarPlot(air.quality.clinton, pollutant = "pm2.5", annotate = "ws", year = 2015, main="PM2.5 varies acoording to wind speed and wind direction in 2015")

# Time plot
timePlot(air.quality.clinton, pollutant = "pm2.5")


# =========== ggmap ========= #
# ggmap
clinton = get_map(location = c(lon = 151.2216, lat = -23.8701), zoom = 16)
clinton.satelite = get_map(location = c(lon = 151.2216, lat = -23.8701), zoom = 16, maptype = "satellite")

# google map layer
ggmap(clinton, extent="device") + 
  theme_bw() +
  annotate("text", x=151.2216, y=-23.8701, label = "Sensor\nlocation", colour = I("red"), size = 3.5) +
  labs(title = "Instrument location at Clinton", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Longitude", 
       y = "Latitude")

# satelite map layer
ggmap(clinton.satelite, extent="device") + 
  theme_bw() +
  annotate("text", x=151.2216, y=-23.8701, label = "Sensor\nlocation", colour = I("red"), size = 3.5) +
  labs(title = "Instrument location at Clinton", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Longitude", 
       y = "Latitude")


## ==================== ## 
## Fitting a Linear Model with Multiple Explanatory Variables 
## ==================== ## 
## fitting four model to see what happen to estimates
# 1. PM2.5 ~ Wind Speed
# 2. PM2.5 ~ Wind direction[s]
# 3. PM2.5 ~ Wind Speed + Wind direction[s]
# 4. PM2.5 ~ Wind speed * wind direction[s] (PM2.5 ~ WS + WD + WS x WD)
# 5. PM2.5 ~ Wind speed : wind direction[s] (PM2.5 ~ WS x WD)

# ======================================== #
## 1. log.pm2.5 ~ Wind Speed (y = B_0 + B_1X1)
lm.pm_ws <- lm(data=air.quality.clinton, log.pm2.5 ~ ws)
lm.pm_ws
# summary of lm
summary(lm.pm_ws)
# Confident Interval
tidy(lm.pm_ws, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_ws <- fortify(lm.pm_ws)
head(df.fort.pm_ws)


# ======================================== #
## 2. log.pm2.5 ~ Wind direction (y = B_0 + B_2X2)
lm.pm_wd <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label - 1)
lm.pm_wd
# summary of lm
summary(lm.pm_wd)
# Confident Interval
tidy(lm.pm_wd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wd <- fortify(lm.pm_wd)
head(df.fort.pm_wd)

# ======================================== #
## 3. log.pm2.5 ~ Wind Speed + Wind direction (y = B_0 + B_1X1 + B_2X2)
lm.pm_ws_wd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws+wd.label - 1)
lm.pm_ws_wd
# summary of lm
summary(lm.pm_ws_wd)
# Confident Interval
tidy(lm.pm_ws_wd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_ws_wd <- fortify(lm.pm_ws_wd)
head(df.fort.pm_ws_wd)


# =========== Including Interaction term ============ #
## 4. log.pm2.5 ~ Wind speed * wind direction  (y = B1X1 + B2X2 + .... + BiX1X2 + BiX1Xi)
lm.pm_ws_wd_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws*wd.label-1)
lm.pm_ws_wd_wswd
# summary of lm
summary(lm.pm_ws_wd_wswd)
# Confident Interval
tidy(lm.pm_ws_wd_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed

df.fort.pm_ws_wd_wswd <- fortify(lm.pm_ws_wd_wswd)
head(df.fort.pm_ws_wd_wswd)


# =========== JUST THE INTERACTION TERMs ========= #
## 5. log.pm2.5 ~ Wind speed : wind direction. (log.pm2.5 ~ WS x WD)  (y = B1X1Z1 + B2X1Z2 + ....)
lm.pm_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws:wd.label -1)
lm.pm_wswd.for.r2 <- lm(data=air.quality.clinton, log.pm2.5 ~ ws:wd.label)
lm.pm_wswd
# summary of lm
summary(lm.pm_wswd)

summary(lm.pm_wswd.for.r2) # for r2 ONLY

# Confident Interval
tidy(lm.pm_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wswd <- fortify(lm.pm_wswd)
head(df.fort.pm_wswd)


# =========== JUST THE INTERACTION TERM ========= #
## 6. log.pm2.5 ~ Wind speed : wind direction. (log.pm2.5 ~ WS x WD)  (y = B_0 + B_1X1Z1 + ....)
lm.pm_wd_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label-1 + ws:wd.label)
lm.pm_wd_wswd.for.r2 <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label + ws:wd.label)
summary(lm.pm_wd_wswd.for.r2) # for r2 ONLY


lm.pm_wd_wswd
# summary of lm
summary(lm.pm_wd_wswd)
# Confident Interval
tidy(lm.pm_wd_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wd_wswd <- fortify(lm.pm_wd_wswd)
head(df.fort.pm_wd_wswd)




# Model fitness
# What is the coefficient of determination, R2, for these models?
r2.check <- function(lm){
  r2 <- broom::glance(lm)$r.squared
  return(r2)
}
# another function to check r2, for linear model with no intercept
r2.noint.check <- function(lm){
  1 - sum(residuals(lm)^2) / sum((air.quality.clinton$log.pm2.5 - mean(air.quality.clinton$log.pm2.5))^2)
}

# df to visualise all the model's r2
Model <- c("M1", "M2", "M3", "M4", "M5", "M6")
R2s <- c(r2.check(lm.pm_ws), 
        r2.noint.check(lm.pm_wd), 
        r2.noint.check(lm.pm_ws_wd),
        r2.noint.check(lm.pm_ws_wd_wswd),
        r2.noint.check(lm.pm_wswd),
        r2.noint.check(lm.pm_wd_wswd))

models.fitness.df <- data.frame(cbind(Model, R2s))
models.fitness.df

## ========== Diagnostics for residuals ========== ##
# residuals of each model into one big dataframe
dat.resid <- data.frame(M1 = df.fort.pm_ws$.resid,
                        M2 = df.fort.pm_wd$.resid,
                        M3 = df.fort.pm_ws_wd$.resid,
                        M4 = df.fort.pm_ws_wd_wswd$.resid,
                        M5 = df.fort.pm_wswd$.resid,
                        M6 = df.fort.pm_wd_wswd$.resid)
head(dat.resid)

# melt the data, and make a histogram
# facet_wrap layer, to split the data into its four models
dat.resid.melt <- melt(dat.resid)
# plot
ggplot(dat.resid.melt, aes(x = value)) +
  geom_histogram(binwidth = 0.5, col="white", alpha=0.7, aes(y = ..density.., fill=variable)) +
  facet_wrap(~variable) + 
  stat_function(fun = dnorm, colour = "grey50", size=1) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="Residuals of each model against normal distribution",
       subtitle="bla bla bla",
       x="Residual",
       y="Density")


# Model 6
# Make a scatter plot of the fitted values vs the residuals, 
# including  a smooth line of best fit to determine whether the residuals have a mean of zero
ggplot(df.fort.pm_wd_wswd, aes(y=.resid, x=.fitted)) +
  geom_point() +
  geom_smooth(se=FALSE) 
# Does it look like there’s unexplained variation in the residuals? Why or why not?


# Make a quantile-quantile plot of the standardised residuals from the interaction model.
ggplot(df.fort.pm_wd_wswd, aes(sample=.stdresid)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0)

# Use the F test via the anova() function to compare both model
anova(lm.pm_wd_wswd, lm.pm_ws_wd_wswd)












## EXPERIMENTAL ZONE ====================

# Fit a null model that only charactersises the average time of flight.
lm.null <- lm(data=air.quality.clinton, log.pm2.5 ~ 1)

# summary of the null model
summary(lm.null)

# What is the sum of square of the errors (residual sum of squares) for the null model?
sum(residuals(lm.null)^2)

# What is the sum of square of the errors (residual sum of squares) for the model involving width?
sum(residuals(lm.pm_ws)^2)

# Use the anova() function to perform the hypothesis test comparing with width model to the null model.
anova(lm.null, lm.pm_ws)

# Make a scatter plot of the fitted values vs the residuals, 
# including  a smooth line of best fit to determine whether the residuals have a mean of zero
ggplot(df.fort.pm_ws, aes(y=.resid, x=.fitted)) +
  geom_point(alpha=0.01) +
  geom_smooth(se=FALSE) 

# Augment the fortified data frame with the WD variable from the df. 
df.fort.pm_ws$wd.label <- air.quality.clinton$wd.label
head(df.fort.pm_ws)

# Plot the residuals against the WD, including  smooth line of best fit to determine whether the residuals have a mean of zero with respect to changes in the WD
ggplot(df.fort.pm_ws, aes(x=wd.label, y=.resid)) + 
  geom_smooth(se=FALSE, method="lm") + 
  geom_point()










# save objects --------------------
# create a new directory for the report stuff
save.image(file="SEB113_CSA_Objects.RData", safe = TRUE)

# ===== END OF SCRIPT =====