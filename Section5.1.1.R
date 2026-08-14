
require(car) # various auxiliary functions including densityPlot

source('auxiliary_functions.R') # auxiliary sampling functions

set.seed(125) # to replicate results

# Read frequency table with independent variables (rows and columns)
freqtbl = read.csv('../data/TwoWayTable.csv')

# Defining the independence
dependencies = list(c(1), c(2))

# # Pearson Chi-square and Likelihood Ratio tests for the independent population table
# crosstbl = xtabs(Freq ~ Row + Column, data=freqtbl)
# (Pearson.full = loglin(crosstbl, dependencies, print=FALSE)$pearson)
# (LR.full = loglin(crosstbl, dependencies, print=FALSE)$lrt)

# Initialize simulation parameters for drawing random subtables
numberOfDraws = 10000    # number of subtables to draw
sampleTableTotal = 2000  # total frequency for each subtable
numberOfRows = 5         # number of rows in each subtable
numberOfColumns = 3      # number of columns in each subtable

# Prepare a dataframe to hold simulation outcomes: Pearson's Chi-squared
# and likelihood ratio statistics for each draw.
simulationResults = data.frame(
    PearsonSampletab = rep(NA, numberOfDraws),
    LRatioSampletab = rep(NA, numberOfDraws)
)

# Execute the simulation loop. For each draw, sample a subtable from the
# dataset, create a crosstab from the sampled data, and compute
# Pearson's Chi-squared and likelihood ratio statistics. Store these
# statistics in the simulationResults dataframe.
# NOTE: Warnings may occur when expected values are small and, therefore,
# the approximation of the p-value may be incorrect. This can happen
# when the sampleTableTotal is not sufficiently large. Uncomment the
# code below to suppress the message.
# suppressWarnings(
for (i in 1:numberOfDraws) {
    subtable = sample_data_subset(freqtbl, noRows=numberOfRows, noCols=numberOfColumns)
    simtable = sample_data_to_crosstab(inputDF=subtable, sampleSize=sampleTableTotal, withReplacement=TRUE)
    simulationResults$PearsonSampletab[i] = loglin(simtable, dependencies, print=FALSE)$pearson
    simulationResults$LRatioSampletab[i] = loglin(simtable, dependencies, print=FALSE)$lrt
}
# )

### RESULTS ###

# Critical values
Chi05 = qchisq(0.05, df=8, lower.tail=FALSE) # 15.50731
Chi01 = qchisq(0.01, df=8, lower.tail=FALSE) # 20.09024

# Pearson's Chi-squares
prop.table(table(simulationResults$PearsonSampletab > Chi05)) # 0.049
prop.table(table(simulationResults$PearsonSampletab > Chi01)) # 0.0106

# Likelihood Ratios
prop.table(table(simulationResults$LRatioSampletab > Chi05)) # 0.051
prop.table(table(simulationResults$LRatioSampletab > Chi01)) # 0.0112

# Density plots
par(mfrow=c(1,2), mar=c(4.5,4.5,4,1))
densityPlot(simulationResults$PearsonSampletab, ylim=c(0.00, 0.12),
    main='Pearson\'s Chi-square', xlab='Statistic')
abline(v=Chi05, col='red', lwd=1.2, lty='dashed')
abline(v=Chi01, col='red', lwd=2.0, lty='solid')
densityPlot(simulationResults$LRatioSampletab, ylim=c(0.00, 0.12),
    main='Likelihood Ratio', xlab='Statistic')
abline(v=Chi05, col='red', lwd=1.2, lty='dashed')
abline(v=Chi01, col='red', lwd=2.0, lty='solid')


