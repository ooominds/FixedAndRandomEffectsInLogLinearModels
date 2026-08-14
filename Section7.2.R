
# Load frequency data for a three-way contingency table
vokeydat = read.csv('../data/Vokey2003.csv')
# Create contingency table: FirstShot × SecondShot × Player
vokeytbl = xtabs(Frequency ~ FirstShot + SecondShot + Player,
    data = vokeydat)
# NOTE: loglin function uses contingency table (vokeytbl),
#       while glm function uses a standard dataframe (vokeydat)
#       to model the Frequency variable

# Fit a log-linear model using the loglin function
# Include all two-way interactions
vokey.ll3 = loglin(vokeytbl,
    list(c('FirstShot', 'SecondShot'), c('FirstShot', 'Player'),c('SecondShot', 'Player')),
    print = FALSE)

# Compute the p-value from the likelihood ratio test for model fit
1 - pchisq(vokey.ll3$lrt, vokey.ll3$df)

# Print the likelihood ratio statistic (G^2)
vokey.ll3$lrt

# Fit the same log-linear model using Poisson regression (glm)
# This model includes the same two-way interactions as above
vokey.ll3.pois <- glm(Frequency ~
    FirstShot : Player +
    SecondShot : Player +
    FirstShot : SecondShot,
    data = vokeydat, family = poisson)

# Compute the p-value for model fit using deviance and residual degrees of freedom
pchisq(deviance(vokey.ll3.pois), df = df.residual(vokey.ll3.pois), lower.tail = FALSE)

# Print the model deviance (equivalent to the likelihood ratio test statistic)
deviance(vokey.ll3.pois)

# =====

# Load the lme4 package for mixed-effects modelling
library(lme4)

# Fit a Poisson mixed-effects model using glmer
vokey.ll3.pois.lme <- glmer(Frequency ~
    0 +                      # Omit the global intercept
    FirstShot : Player +     # Fixed effect: interaction between FirstShot and Player
    SecondShot : Player +    # Fixed effect: interaction between SecondShot and Player
    FirstShot : SecondShot + # Fixed effect: interaction between FirstShot and SecondShot
    (1 | Player),            # Random intercept for Player
    data = vokeydat, family = poisson) # Use Poisson distribution for count data

# Output the model deviance (equivalent to the likelihood ratio test statistic)
deviance(vokey.ll3.pois.lme)

