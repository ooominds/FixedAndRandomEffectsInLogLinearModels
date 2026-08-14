
########################################
### Example 1: Dancer et al., (1994) ###
########################################

# Read frequency dataframe
dancerdat = read.csv('../data/DancerEtAl1994.csv')

# Create contingency table: Response × MentalHealthStatus × EthnicGroup
dancertbl = xtabs(Frequency ~ Response + MentalHealthStatus + EthnicGroup,
    data = dancerdat)

# Fit log-linear model with only main effects (complete independence)
# and get model fit p-value; a small p-value indicates a poor fit
dancer.ll1 = loglin(dancertbl,
    list(c('Response'), c('MentalHealthStatus'), c('EthnicGroup')),
    print = FALSE)
1 - pchisq(dancer.ll1$lrt, dancer.ll1$df)

# Add conditional dependence between MentalHealthStatus and EthnicGroup
dancer.ll2 = loglin(dancertbl,
    list(c('Response'), c('MentalHealthStatus', 'EthnicGroup')),
    print = FALSE)
1 - pchisq(dancer.ll2$lrt, dancer.ll2$df)

# Add conditional dependence between Response and MentalHealthStatus
dancer.ll3 = loglin(dancertbl,
    list(c('Response', 'MentalHealthStatus'), c('MentalHealthStatus', 'EthnicGroup')),
    print = FALSE)
1 - pchisq(dancer.ll3$lrt, dancer.ll3$df)

###############################
### Example 2: Vokey (2003) ###
###############################

# Load frequency data for a three-way contingency table
vokeydat = read.csv('../data/Vokey2003.csv')

# Create contingency table: FirstShot × SecondShot × Player
vokeytbl = xtabs(Frequency ~ FirstShot + SecondShot + Player,
    data = vokeydat)

# Fit log-linear model with only main effects (complete independence)
# and get model fit p-value; a small p-value indicates poor fit
vokey.ll1 = loglin(vokeytbl,
    list(c('FirstShot'), c('SecondShot'), c('Player')),
    print = FALSE)
1 - pchisq(vokey.ll1$lrt, vokey.ll1$df)  # p-value for model fit

# Add conditional dependence between FirstShot and SecondShot
vokey.ll2 = loglin(vokeytbl,
    list(c('FirstShot', 'SecondShot'), c('Player')),
    print = FALSE)
1 - pchisq(vokey.ll2$lrt, vokey.ll2$df)

# Add conditional dependence between FirstShot and Player, and SecondShot and Player
vokey.ll3 = loglin(vokeytbl,
    list(c('FirstShot', 'SecondShot'), c('FirstShot', 'Player'),c('SecondShot', 'Player')),
    print = FALSE)
1 - pchisq(vokey.ll3$lrt, vokey.ll3$df)

