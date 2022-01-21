library("fpp3")
#using regression as a modeling technique

# Consumption and Income closely track each other
us_change %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Consumption")) +
  geom_line(aes(y = Income, colour = "Income")) +
  ylab("% change") + xlab("Year") +
  guides(colour=guide_legend(title="series"))

# You can see the relationship in a scatter plot
us_change %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# The tslm function fits a regression model with time series data
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  report()

# In fact there are several relationships in the data
us_change %>%
  GGally::ggpairs(columns = 2:6)

# and we can use tslm to fit a multiple regression model
fit.consMR <- us_change %>%
  model(
    tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings)
  )
report(fit.consMR)

# With time series regression it is always good to check the fit as a time series plot
augment(fit.consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  xlab("Year") + ylab(NULL) +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=NULL))

# and we can do an actual by predicted plot too
augment(fit.consMR) %>%
  ggplot(aes(x=Consumption, y=.fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

# and check the residuals for auto-correlation
fit.consMR %>% gg_tsresiduals()
augment(fit.consMR) %>% features(.resid, ljung_box, lag = 10, dof = 5)

# The usual residual plots are also a good idea
df <- left_join(us_change, residuals(fit.consMR), by = "Quarter")
ggplot(df, aes(x=Income, y=.resid)) +
  geom_point() + ylab("Residuals")
ggplot(df, aes(x=Production, y=.resid)) +
  geom_point() + ylab("Residuals")
ggplot(df, aes(x=Savings, y=.resid)) +
  geom_point() + ylab("Residuals")
ggplot(df, aes(x=Unemployment, y=.resid)) +
  geom_point() + ylab("Residuals")

augment(fit.consMR) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

# Recall that the beer production data had trend and seasonality
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
recent_production %>%
  autoplot(Beer) +
  labs(x = "Year", y = "Megalitres")

# In tslm, trend and seasona are keywords that can be used to create a regression model
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = "Megalitres",
       title = "Quarterly Beer Production")

augment(fit_beer) %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# When you have a good model, the forecast function can be applied to the fitted model
fc_beer <- forecast(fit_beer)
fc_beer %>%
  autoplot(recent_production) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

# Sometime non-linear models will be more useful
boston_men <- boston_marathon %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

boston_men %>%
  autoplot(Minutes) +
  xlab("Year") +  ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour=guide_legend(title="Model"))

fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980)))
  )
fc_trends <- fit_trends %>% forecast(h=10)

boston_men %>%
  autoplot(Minutes) +
  geom_line(aes(y=.fitted, colour=.model), data = fitted(fit_trends)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  xlab("Year") +  ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour=guide_legend(title="Model"))
