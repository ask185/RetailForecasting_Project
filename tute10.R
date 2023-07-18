library(fpp3)


# To make the data stationary

# 1. Transform the data:

aus_production %>%
  autoplot(log(Electricity))

# we do the log transformation because the data looked highly varied
# but we can tell that the transformation is too strong because
# the curve is nonlinear and the variance becomes smaller toward the end
# of the series

## Too strong a transformation (lambda =0)
# let's try out the box-cox transformation.

aus_production %>%
  autoplot(box_cox(Electricity, lambda = 0.4))

## trying out the 0.6 was too weak as the variance was 
# still not properly standardized. WE can try out the guerrerro 
# method to see the recommended value of lambda.

aus_production %>%
  features(Electricity, features = lst(guerrero))

# guerrero recommends the value of 0.531.

# 2. Seasonally difference the data - if the data is seasonal

aus_production %>%
  autoplot(box_cox(Electricity, lambda = 0.4))

# since this data is seasonal we should consider seasonal difference

aus_production %>%
  autoplot(difference(box_cox(Electricity, lambda = 0.4),
                      lag =4)) # since it's quarterly data, I will difference
# it quarterly
# the seasonal transformation is not so impressive because the
# variance is not even across the series.
# not using the guerrero recommended lambda value because the difference is not 
# strong enough to stabilize the variance completely

# 3. Regular difference (if non-stationary)

# to check if the data is non-stationary, we use the tsdisplay

aus_production %>%
  gg_tsdisplay(difference(box_cox(Electricity, lambda = 0.4),
                          lag=4),
               plot_type = "partial")

# the PACF plots looks stationary because only the first lag is
# significant (large and positive value) and ACF plot decays quickly

# if still in doubt we can do the unit root test;


aus_production %>%
  features(
    difference(
      box_cox(Electricity, lambda = 0.4),
      lag=4),
    features=lst(unitroot_kpss, unitroot_ndiffs))

# the kpss test shows that the data is differenced and the 
# number of differencing required to make this data stationary is
# 1 more, so lets that.

aus_production %>%
  gg_tsdisplay(
    difference(
      difference(
        box_cox(Electricity, lambda = 0.4),
        lag=4)),
    plot_type = "partial")

# seems really good

# for the c part we have to identify ARIMA Models manually by focusing on the
# ACF and PACF, and estimate these models and choose the best one according to the 
# AICc values; last week ACF and PACF were used to identify the AR order -- the
# lags of AR components and the lags of the MA component.
# in this tutorial we will extend that a little bit to include seasonal lags and look at 
# seasonal lags and look at the seasonal ACF components; we can use the 
# ACF to determine the MA(q) component and we can use PACF to determine the AR(p) 
# component. The method we used was considering the last significant lag.
# From the ACF the first lag is significant, second and third are insignificant and the
# fourth is significant and the seventh is just significant and anything after
# this is just chance. From the last week we only considered the last significant lag
# given that we were not considering the seasonal component. However, since we are
# doing the seasonal ARIMA models, we consider seasonal lags differently.
# lag 4 is significant because this is a quarterly data.When we consider the 
# seasonal ARIMA model, we consider both the lower case "q" and the "Q".This can be denoted
# like ARIMA(p,d,q)(P,D,Q)[m] m-- the seasonal period and the P,D,Q are for the
# seasonal ARIMA.The seasonal lag = 4 represents the seasonal lag which determines the 
# "Q". And the non-seasonal lag that is lag = 1 determines the "q", so we are really working with
# the MA(1) because the seasonal lag (lag=4) will be handled by the "Q".

# Let's build up the model. Up until this point, we have done 1 non-seasonal difference and 1
# seasonal difference. From the MA component we see that there might be 1 "q" --
# 1 non-seasonal significant lag (lag =1) from the ACF and the seasonal lag (lag=4) suggests there
# is also 1 significant seasonal lag (we could also consider two seasonal lags because the lag =8 is 
# a little bit significant), so the seasonal (S) MA model SMA = 1.

# Previously discussed that it is difficult to see the interactions between the AR and MA components of ARMA
# Bur when we choose a model looking at the MA component solely we immediately discount
# the AR component.We do not consider any lags from the PACF to be interpret able so we do not include them in our model
# therefore, the "p" and "P" are both 0. ARIMA(0,1,1)(0,1,1)[4] => we are considering the moving average model by
# looking at the ACF. Looking at the ACF this is one seasonal ARIMA model that we might consider. Now we can also choose
# model by looking at the PACF and ignoring the ACF. 

# When we discount the ACF (MA component) and focus on PACF (AR component) we can our seasonal ARIMA model as ARIMA(p,1,0)(P,1,0)[4]
# looking at PACF to identify the "p", and "P" -- lag order of the AR and seasonal AR, the same idea applies here.                    
# we can see that the 1 lag is significant, the second and third are not significant but the fourth lag is significant; this however
# is the seasonal Hence we ignore the seasonal lag (lag=4) when consider the "p" -- the non-seasonal lag. Usually we ignore the lags                            
# after the first seasonal lag even if they are significant. There are various reasons for this:
# the first is that there will be interaction between the AR and seasonal AR component, so lag=1 and lag=4 interact give lag=5.
# this is will be seen in the equation soon. p will be considered to be equal to 1 here. SAR, lags = 4, 8, 12, 16 are all significant
# so the last significant seasonal lag is lag=16 and 4 lags actually from lags =4,8,12,16. This means we set the SAR(P=4) and hence the 
# ARIMA(1,1,0)(4,1,0)[4]. However so far we have been unable to consider any interaction between these terms.WE need to shortlist a few 
# potential model.Potential model can be attained by just mixing the two model. 
ARIMA(Electricity ~ 0+ pdq(0,1,1) + PDQ(P=0,D=1,Q=1)) # only MA components considered
ARIMA(Electricity ~ 0+ pdq(1,1,0) + PDQ(P=4,D=1,Q=0)) # only AR components considered
ARIMA(Electricity ~ 0+ pdq(1,1,1) + PDQ(P=1,D=1,Q=1)) # 1 AR non-seasonal and 1 AR seasonal + 1 MA non-seasonal and 1 MA seasonal components 
ARIMA(Electricity ~ 0+ pdq(0,1,1) + PDQ(P=2,D=1,Q=1)) # 0 AR non-seasonal and 2 AR seasonal components + 1 non-seasonal MA and 1 MA seasonal
#component.
# Lets get back to the exercise: we model them all and choose which ones best using AIC
# Keep in mind that we are not modeling Electricity in its original form we are in fact considering the log-transformed version of it.

ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=0,D=1,Q=1))
ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,0) + PDQ(P=4,D=1,Q=0))
ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,1) + PDQ(P=1,D=1,Q=1))
ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=2,D=1,Q=1))


aus_production %>%
  model(
    arima011011 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=0,D=1,Q=1)),
    arima110410 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,0) + PDQ(P=4,D=1,Q=0)),
    arima111111 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,1) + PDQ(P=1,D=1,Q=1)),
    arima011211 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=2,D=1,Q=1))
    
  ) -> fit

# for one row summary of all these models: we use the glance
fit %>% glance()

# We look at the model that has the minimum AIC and that is the ARIMA(111)(111) model.This model has interaction between the AR and MA.
# Let's also consider the auto model by the ARIMA:

aus_production %>%
  model(
    auto_arima = ARIMA(box_cox(Electricity, lambda = 0.4)),
    arima011011 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=0,D=1,Q=1)),
    arima110410 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,0) + PDQ(P=4,D=1,Q=0)),
    arima111111 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,1) + PDQ(P=1,D=1,Q=1)),
    arima011211 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=2,D=1,Q=1))
    
  ) -> fit

fit %>% glance()
fit$auto_arima

# the model chosen by the ARIMA is (1,1,4)(0,1,1): 1 AR, 1 difference-ing, 4 MA, and 0 SAR, 1 seasonal difference, 1 SMA 
# Let's compare the auto and our best model manually done: we compare by using gg_tsresiduals()

fit%>%
  select(arima111111) %>%
  gg_tsresiduals()
# residuals for the most part are white noise.Histogram looks fairly symmetric. Let's forecast for the next two years a head. 
fc <- fit %>%
  select(arima111111) %>%
  forecast(h = "2 years")

fc %>%
  autoplot(aus_production) # as visible from the plot, the seasonality seems to have adopted to recent changes in seasonal structure
# of the data. lets also try the ets. For ets we do not have to do the trasnformation because ETS can handle multiplicative components
aus_production %>%
  model(
    ets_auto = ETS(Electricity),
    auto_arima = ARIMA(box_cox(Electricity, lambda = 0.4)),
    arima011011 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=0,D=1,Q=1)),
    arima110410 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,0) + PDQ(P=4,D=1,Q=0)),
    arima111111 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,1) + PDQ(P=1,D=1,Q=1)),
    arima011211 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=2,D=1,Q=1))
    
  ) -> fit

# the model chosen by the ets is MAM model that is multiplicative errors, additive trend and multiplicative seasonality
# using the glance fucntion we can see which of these models do well

fit %>% glance()

#  we cannot compare the ets and ARIMA models because they are on different scales: ARIMA works with transformed response variable
# while the ets is using the original variable; also there are strict conditions surrounding when AIC can be used to make the comparisons
# of the model classes. Note: if in the auto model we force d=0 (no regular differencing) then we still cannot compare it to other models
# since all other models have the same number of differences and for the arima models to be compared we need to have the same # of differences 
# for each model
aus_production %>%
  model(
    ets_auto = ETS(Electricity),
    auto_d0_arima = ARIMA(box_cox(Electricity, lambda = 0.4) ~ pdq(d=0)),
    auto_arima = ARIMA(box_cox(Electricity, lambda = 0.4)),
    arima011011 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=0,D=1,Q=1)),
    arima110410 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,0) + PDQ(P=4,D=1,Q=0)),
    arima111111 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(1,1,1) + PDQ(P=1,D=1,Q=1)),
    arima011211 = ARIMA(box_cox(Electricity, lambda = 0.4) ~ 0 + pdq(0,1,1) + PDQ(P=2,D=1,Q=1)) 
    
  ) -> fit
# we can evaluate this using different methods, perhaps accuracy. Let's just focus on the auto arima model
fit %>%
  select(auto_arima) %>%
  gg_tsresiduals()

fit %>%
  select(ets_auto) %>%
  gg_tsresiduals()

# white noise residuals in ACF and comparing that with the auto ets model: although the acf of residuals is white noise 
# there are a few lags closed to the blue dashed line. And if we carry out the formal tests: this auto-ets model would fail the test because
# it appears there sill might be some information in the residuals that ets-auto needs to capture.
# based on the residuals diagnostics the ARIMA auto model is performing better than the ets-auto
# for the arima model the ACF for residuals is complete white noise and the histogram is much more symmetric & normally distributed. 
# but lets compare the forecasts from these two models:

fit %>%
  select(ets_auto,auto_arima) %>%
  forecast(h="10 years") %>%
  autoplot(tail(aus_production, 4*10), alpha=0.3)+ theme_bw()
# ets and arima models are behaving very similar but the interval for arima is slightly smaller.
# arima model has better residuals diagnostics which suggests that the intervals are more reliable. 
# we can write this model using the back-shift notation:
# Question 12: 

aus_production %>%
  model(STL(box_cox(Electricity, 0.4))) %>%
  components() %>%
  autoplot(season_adjust)

elec_dcmp <- aus_production %>%
  model(STL(box_cox(Electricity, 0.4))) %>%
  components()%>%
  select(-.model)


fit_dcmp <- elec_dcmp %>%
  model(
    ARIMA(season_adjust)
  )

fit_dcmp %>%
  forecast(h="10 years") %>%
  autoplot(elec_dcmp)

# Question 15

pelt %>%
  autoplot(Hare)

# this data set is already stationary -- already discussed in earlier tutes 
pelt %>%
  gg_tsdisplay(Hare, plot_type = "partial")

# the ACF has sinusoidal shape and the lag 1 is significant 
# for PACF plot, we see that the lags 1 and 2 are significant
# We have considered AR(4) which comes from PACF, so we ignore the ACF entirely.Why have
# lag 4 as being the appropriate lag order for this data set.
# so here we are looking for the last significant lag; lag is marginally significant
# the best model for this would be some combination of the AR and MA. 
# d -- make forecast for the next three years given the parameters and the last five observations
# the calculations done on the notebook.  
# 
# 
# 
# 
# 
#