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
pacman::p_load(ggplot2, dplyr, openair, GGally, reshape2, broom, lubridate, ggmap, gridExtra) # pacman to install/load libraries
# latest ggplot2 needed to render subtitle, devtools::install_github("hadley/ggplot2")
# ===== Dependency check =====

## read raw csv file from cwd
# air.quality.clinton.raw <- read.csv(file="data/clinton-aq-2015.csv", as.is=T, head=T)

# Or read directly from data source.
url <- "http://www.ehp.qld.gov.au/data-sets/air-quality/clinton-aq-2015.csv"
air.quality.clinton.raw <- read.csv(file=url, as.is=T, head=T)

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

## Create new variable log.pm2.5 and wd.label
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

## ==================== ## 
## Exploratory Data Visualisation
## ==================== ## 

# summarise variables
summary(air.quality.clinton[,c(6,8,9)])

# Plot the pm2.5 concentration in air in histrogram
pm1 <- ggplot(data=air.quality.clinton, aes(x=pm2.5)) + 
  geom_histogram(binwidth = 0.1, col="#CD5D67", fill="#CD5D67", alpha=0.4) +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title=expression(paste("Before: ",PM[2.5]," data skewed")),
       subtitle="bla bla bla",
       x= expression(paste(PM[2.5]," (",mu,g,m^-3,")")),
       y= "Count")

# Plot the pm2.5 concentration in air in histrogram with log scale
pm2 <- ggplot(data=air.quality.clinton, aes(x=log.pm2.5)) + 
  geom_histogram(binwidth = 0.1, col="#61C9A8", fill="#61C9A8", alpha=0.7) +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="After log transformation",
       subtitle="bla bla bla",
       x= expression(paste(log," ",PM[2.5]," (",mu,g,m^-3,")")),
       y= "")

grid.arrange(pm1, pm2, ncol=2)


# Plot the wind direction in pie chart
(wind_direction.pie <- 
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
)

# histogram of ws
(ws.histogram <- ggplot(data=air.quality.clinton, aes(x=ws)) + 
  geom_histogram(binwidth = 0.1, col="#1789FC", fill="#1789FC", alpha=0.6) +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="Wind speed distribution",
       subtitle="Site location: Latitude: -23.8701; Longitude: 151.2216",
       x= expression(paste("Wind Speed (",ms^-1,")")),
       y= "Count")
)

# ws vs wd
ws_wd.plot <- ggplot(data=air.quality.clinton, aes(x=wd.label, y=ws)) +
  geom_boxplot(outlier.colour = NULL, aes_string(colour="wd.label", fill="wd.label")) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title = "Wind speed vs Wind direction", 
       subtitle = "further split it into 8 different wind direction facet", 
       y = expression(paste("Wind speed, ", ms^-1)), 
       x = "Wind Direction")
# use the details of the plot, to derive the coordinates of where the median line is,
# and then add colour to it using geom_segment.
dat <- ggplot_build(ws_wd.plot)$data[[1]]
(ws_wd.plot <- 
  ws_wd.plot + geom_segment(data=dat, 
                            aes(x=xmin, xend=xmax, y=middle, yend=middle), 
                            colour="white", size=0.8)
)

# Plot the pm2.5 concentration varies according to wind direction in boxplot
pm_wd.plot <- 
  ggplot(data=air.quality.clinton, aes(x=wd.label, y=log.pm2.5)) + 
  geom_boxplot(outlier.colour = NULL, aes_string(colour="wd.label", fill="wd.label")) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title = "PM2.5 concentration varies according to wind direction", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Wind Direction", 
       y = expression(paste(log," ",PM[2.5]," (",mu,g,m^-3,")")))
# use the details of the plot, to derive the coordinates of where the median line is,
# and then add colour to it using geom_segment.
dat <- ggplot_build(pm_wd.plot)$data[[1]]
(pm_wd.plot <- 
  pm_wd.plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), 
                            colour="white", size=0.8)
)


# ws and pm
(pm_ws.plot <- ggplot(data=air.quality.clinton, aes(x=ws, y=log.pm2.5)) +
  geom_point( size=8, alpha=0.1, color="#545E56") +
  geom_smooth(method="lm", se=FALSE, color="#FF6978", alpha=0.6, size=0.5) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title = "PM2.5 concentration plot against Wind Speed", 
       subtitle = "", 
       x = expression(paste("Wind Speed (",ms^-1,")")), 
       y = expression(paste(log," ",PM[2.5]," (",mu,g,m^-3,")")))
)

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
(pm_ws_wd.plot <- ggplot(data=air.quality.clinton, aes(x=ws, y=log.pm2.5)) +
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
  labs(title = "PM2.5 concentration varies with Wind speed and Wind Direction", 
       subtitle = "further split it into 8 different wind direction facet", 
       x = expression(paste("Wind Speed (",ms^-1,")")), 
       y = expression(paste(log," ",PM[2.5]," (",mu,g,m^-3,")"))
       )
)


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
(lm.pm_wd <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label - 1))
(lm.pm_wd.intercept <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label))

# summary of lm
summary(lm.pm_wd)
summary(lm.pm_wd.intercept)
# Confident Interval
tidy(lm.pm_wd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wd <- fortify(lm.pm_wd)
head(df.fort.pm_wd)

# ======================================== #
## 3. log.pm2.5 ~ Wind Speed + Wind direction (y = B_0 + B_1X1 + B_2X2)
(lm.pm_ws_wd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws+wd.label - 1))
(lm.pm_ws_wd.intercept <- lm(data=air.quality.clinton, log.pm2.5 ~ ws+wd.label))
# summary of lm
summary(lm.pm_ws_wd)
summary(lm.pm_ws_wd.intercept)
# Confident Interval
tidy(lm.pm_ws_wd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_ws_wd <- fortify(lm.pm_ws_wd)
head(df.fort.pm_ws_wd)


# =========== JUST THE INTERACTION TERMs ========= #
## 4. log.pm2.5 ~ Wind speed : wind direction. (log.pm2.5 ~ WS x WD)  (y = B1X1Z1 + B2X1Z2 + ....)
(lm.pm_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws:wd.label -1))
(lm.pm_wswd.intercept <- lm(data=air.quality.clinton, log.pm2.5 ~ ws:wd.label))
# summary of lm
summary(lm.pm_wswd)
summary(lm.pm_wswd.intercept) # for r2 ONLY

# Confident Interval
tidy(lm.pm_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wswd <- fortify(lm.pm_wswd)
head(df.fort.pm_wswd)


# =========== WD AND THE INTERACTION TERM, THE BEST ========= #
## 5. log.pm2.5 ~ Wind direction + Wind speed : wind direction. (log.pm2.5 ~ WD + WS x WD)  (y = B_0 + WD + B_1X1Z1 + ....)
(lm.pm_wd_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label-1 + ws:wd.label))
(lm.pm_wd_wswd.intercept <- lm(data=air.quality.clinton, log.pm2.5 ~ wd.label + ws:wd.label))
# summary of lm
summary(lm.pm_wd_wswd)
summary(lm.pm_wd_wswd.intercept) # for r2 ONLY
# Confident Interval
tidy(lm.pm_wd_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed
df.fort.pm_wd_wswd <- fortify(lm.pm_wd_wswd)
head(df.fort.pm_wd_wswd)


# =========== Both terms and with Interaction term ============ #
## 6. log.pm2.5 ~ Wind speed * wind direction  (y = B1X1 + B2X2 + .... + BiX1X2 + BiX1Xi)
(lm.pm_ws_wd_wswd <- lm(data=air.quality.clinton, log.pm2.5 ~ ws*wd.label-1))
(lm.pm_ws_wd_wswd.intercept <- lm(data=air.quality.clinton, log.pm2.5 ~ ws*wd.label))
# summary of lm
summary(lm.pm_ws_wd_wswd)
summary(lm.pm_ws_wd_wswd.intercept)
# Confident Interval
tidy(lm.pm_ws_wd_wswd, conf.int = T)
# Check lm whether the residuals are normally distributed

df.fort.pm_ws_wd_wswd <- fortify(lm.pm_ws_wd_wswd)
head(df.fort.pm_ws_wd_wswd)




# Model fitness
# What is the coefficient of determination, R2, for these models?
r2.check <- function(lm){
  r2 <- broom::glance(lm)$r.squared
  return(r2)
}

# another function to check r2, for linear model with no intercept
# r2.noint.check <- function(lm){
#   1 - sum(residuals(lm)^2) / sum((air.quality.clinton$log.pm2.5 - mean(air.quality.clinton$log.pm2.5))^2)
# }

# df to visualise all the model's r2
Model <- c("M1", "M2", "M3", "M4", "M5", "M6")

R2s <- c(r2.check(lm.pm_ws), 
        r2.check(lm.pm_wd.intercept), 
        r2.check(lm.pm_ws_wd.intercept),
        r2.check(lm.pm_wswd.intercept),
        r2.check(lm.pm_wd_wswd.intercept),
        r2.check(lm.pm_ws_wd_wswd.intercept)
        )

(models.fitness.df <- data.frame(cbind(Model, R2s)))

## ========== Diagnostics for residuals ========== ##
# residuals of each model into one big dataframe
dat.resid <- data.frame(M1 = df.fort.pm_ws$.resid,
                        M2 = df.fort.pm_wd$.resid,
                        M3 = df.fort.pm_ws_wd$.resid,
                        M4 = df.fort.pm_wswd$.resid,
                        M5 = df.fort.pm_wd_wswd$.resid,
                        M6 = df.fort.pm_ws_wd_wswd$.resid)
head(dat.resid)

# melt the data, and make a histogram
# facet_wrap layer, to split the data into its four models
dat.resid.melt <- melt(dat.resid)
# plot
all.resid.histogram <- ggplot(dat.resid.melt, aes(x = value)) +
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


# Residual histogram
Resid.histogram <- ggplot(df.fort.pm_wd_wswd, aes(x = .resid)) +
  geom_histogram(binwidth = 0.5, col="white", alpha=0.7, fill="#4981D1", aes(y = ..density..)) +
  stat_function(fun = dnorm, colour = "grey50", size=1) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="Residuals from the model 5 plot against Normal Distribution",
       subtitle="Histogram of the residuals with density on the y axis with assumption of normal distribution of the residuals",
       x="Residual",
       y="Density")

# Model 5
# Make a scatter plot of the fitted values vs the residuals, 
# including  a smooth line of best fit to determine whether the residuals have a mean of zero
homoplot <- ggplot(df.fort.pm_wd_wswd, aes(y=.resid, x=.fitted)) +
  geom_point(aes(color=wd.label), alpha=0.3) +
  geom_smooth(se=FALSE, color="blue", alpha=0.6, size=0.5) +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="Homogeneity of errors",
       subtitle="Residuals doesn't look like the variance stays the same as we move from left to right along the fitted values axis.",
       x="Fitted value",
       y="Residuals")
# Does it look like there’s unexplained variation in the residuals? Why or why not?


# Make a quantile-quantile plot of the standardised residuals from the interaction model.
qqplot <- ggplot(df.fort.pm_wd_wswd, aes(sample=.stdresid)) +
  stat_qq(geom="point", color="#00CC99", size=5, alpha=0.3) +
  # geom_abline(xintercept=0, slope=1, lty=2)
  geom_abline(intercept=0, slope=1, lty=2, color="#FF1654", size=0.8) + # xintercept depreciated 
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(lineheight=.8, 
                                  face="bold")) +
  labs(title="Normality of the residuals, \nquantile-quantile (QQ) plot of the standardised residuals.",
       subtitle="The quantiles of the values are plotted against the quantiles of a standard Normal.",
       x="Theoretical (Z ~ N(0,1))",
       y="Sample")


# Create a scatterplot of the fitted vs observed data (yhat vs y), 
# including a line showing where the yhat and y are equal.
goodnessplot <- ggplot(data=df.fort.pm_wd_wswd, aes(y=log.pm2.5, x=.fitted)) + 
  geom_point(aes(color=wd.label), alpha=0.3, size=3) + 
  geom_abline(intercept=0, slope=1) +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, 
                                  face="bold"),
        strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  labs(title="Fitted vs observed data (yhat vs y)",
       subtitle="Line showing where the yhat and y are equal.",
       x="Fitted value (yhat)",
       y=expression(paste(log," ",PM[2.5]," (observed data)")))


# ==== ANOVA ====
# Use the F test via the anova() function to compare both model

# model 4 and model 5 has diff r2, but close. Anova to compare both of them
# model 4 nested inside of model 5
# use the fitted model with intercept when using anova comparison
models.fitness.df
anova(lm.pm_wswd.intercept, lm.pm_wd_wswd.intercept)

# Model 5 vs. Model 6, model 5 nested in model 6
# Same r2 but model 6 has extra term (ws)
anova(lm.pm_wd_wswd.intercept, lm.pm_ws_wd_wswd.intercept)


# =========== ggmap ========= #
# ggmap
clinton = get_map(location = c(lon = 151.2216, lat = -23.8701), zoom = 16)
clinton.satelite = get_map(location = c(lon = 151.2216, lat = -23.8701), zoom = 16, maptype = "satellite")

# google map layer
sensor_map <- ggmap(clinton, extent="device") + 
  geom_point(aes(x=151.2216, y=-23.8701), color="red", size=7, alpha=0.05) +
  theme_bw() +
  annotate("text", x=151.2216, y=-23.8701, label = "Sensor", colour = I("red"), size = 3.5) +
  labs(title = "Instrument location at Clinton", 
       subtitle = "Site location: Latitude: -23.8701; Longitude: 151.2216", 
       x = "Longitude", 
       y = "Latitude")


# save objects --------------------
# create a new directory for the report stuff
save.image(file="data/SEB113_CSA_Objects.RData", safe = TRUE)

# ===== END OF SCRIPT =====