
require(car) # various auxiliary functions including densityPlot

source('auxiliary_functions.R') # auxiliary sampling functions

set.seed(125) # to replicate results

# Even categories (between 46% and 54%) are considered
evenCat = c(12, 20, 41, 49, 50, 56, 57, 59, 63, 64, 65, 68, 71, 72, 73, 78)

# Complete ONS table that is pruned, retaining only even categories
inputFreqtbl = read.csv('../data/RM053-2021-1-filtered-2023-09-09T17_03_40Z.csv')
freqtbl = droplevels(na.omit(inputFreqtbl[!(inputFreqtbl$Frequency<50) &
    (inputFreqtbl$IndustryCode %in% evenCat) & !(inputFreqtbl$NationalIdentityCode==-8),]))
freqtbl = freqtbl[,c(1,4,7)]

# #######################################
# ### RUN THIS PART ON A SERVER / HPC ###
# #######################################
# 
# # NOTE: This section of the code is computationally intensive and is
# # best run on a server or HPC cluster to leverage greater computational
# # resources and improve execution speed.  It might still take between
# # 10-20 hours.
# 
# # Defining the independence
# dependencies = list(c(1), c(2))
# 
# # Initialize simulation parameters for drawing random subtables
# numberOfDraws = 10000    # number of subtables to draw
# sampleTableTotal = 500   # total frequency for each subtable
# 
# # Prepare a dataframe to hold simulation outcomes: number of rows and
# # columns of a crosstab, Pearson's Chi-squared and likelihood ratio
# # statistics for each draw.
# simulationResults6 = data.frame(
#     NumberOfRows = rep(NA, numberOfDraws),
#     NumberOfColumns = rep(NA, numberOfDraws),
#     PearsonSampletab = rep(NA, numberOfDraws),
#     LRatioSampletab = rep(NA, numberOfDraws)
# )
# 
# # Execute the simulation loop. For each draw, sample a subtable from the
# # dataset. The size of the subset varies for each draw, randomly chosen
# # from the range of available categories. If the resulting crosstable is
# # too small (less than 4 elements), the draw is skipped. Otherwise,
# # statistical metrics are calculated. Results include the dimensions of
# # the crosstable and calculated Pearson's Chi-squared and likelihood
# # ratio statistics. They are stored in the simulationResults6
# # dataframe.
# # NOTE: Warnings may occur when expected values are small and, therefore,
# # the approximation of the p-value may be incorrect. This can happen
# # when the sampleTableTotal is not sufficiently large. Uncomment the
# # code below to suppress the message.
# # suppressWarnings(
# countSkipped = 0
# for (i in 1:numberOfDraws) {
#     subsetSize = sample(2:length(evenCat), 1)
#     simtable = filter_data_by_values(inputDF=freqtbl, filterColumn='IndustryCode', filterValues=evenCat, subsetSize=subsetSize)
#     crosstable = sample_data_to_crosstab(inputDF=simtable, sampleSize=sampleTableTotal, withReplacement=TRUE)
#     crosstable = crosstable[rowSums(crosstable < 2) == 0,]
#     if (length(crosstable) < 4) {
#         countSkipped = countSkipped + 1
#     } else {
#         simulationResults6$NumberOfRows[i] = dim(crosstable)[1]
#         simulationResults6$NumberOfColumns[i] = dim(crosstable)[2]
#         simulationResults6$PearsonSampletab[i] = loglin(crosstable, dependencies, print=FALSE)$pearson
#         simulationResults6$LRatioSampletab[i] = loglin(crosstable, dependencies, print=FALSE)$lrt
#     }
#     cat(paste0(i, '\n')) # track progress
# }
# # )
# 
# # Output the total number of skipped simulations
# cat(paste0(countSkipped, ' simulations were skipped.\n'))
# 
# 
# # Save results
# save(simulationResults6, file='simulationResults6.rda')
# 
# ###################################################
# ### END OF THE PART TO BE RUN ON A SERVER / HPC ###
# ###################################################

### RESULTS ###

# Load results
load('../results/simulationResults6.rda')

# Number of tables by row sizes
table(simulationResults6$NumberOfRows)
#   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
# 647 694 703 697 744 791 791 797 847 851 915 821 526 161  13 

# Number of tables by column sizes
table(simulationResults6$NumberOfColumns)
#    2 
# 9998 

# Initialize a list to store the proportion of tables of different
# degrees of freedom where Pearson Chi-squared statistics exceed the
# critical value at 5%.
howMany = list()
for (cols in 2:16) {
    currDF = cols * 2 - (cols-1) - (2-1) - 1
    currChi05 = qchisq(0.05, df=currDF, lower.tail=FALSE)
    currGreater = prop.table(table(simulationResults6$PearsonSampletab[simulationResults6$NumberOfRows==cols] > currChi05))
    if ('TRUE' %in% names(currGreater)) { # safeguarding for the FALSE only outcome
        currGreater = currGreater[[which(names(currGreater)=='TRUE')]]
    } else {
        currGreater = 0.0
    }
    howMany[[paste(cols)]] = currGreater
}

# Output the proportions, rounded to three decimal places.
round(unlist(howMany), 3)
#     2     3     4     5     6     7     8     9    10    11 
# 0.102 0.104 0.119 0.105 0.082 0.085 0.073 0.070 0.063 0.065 
#    12    13    14    15    16 
# 0.056 0.054 0.038 0.068 0.000 

# Define colours and lines for plotting
myColours = c('#333333', '#ef3b2c', '#33a02c', '#6a3d9a', '#08519c',
    '#a65628', '#888888', '#b30000', '#b2df8a', '#ff7f00', '#cab2d6',
    '#377eb8', '#bdbdbd', '#225ea8', '#8c510a', '#54278f', '#80cdc1',
    '#252525', '#662506')
myLines = c('solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid',
    'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dotted',
    'dotted', 'dotted', 'dotted', 'dotted', 'dotted')

# Filter the simulationResults6 dataframe to include only those
# simulations with fewer than 16 rows, and use droplevels() to remove
# unused levels in factor columns, if any. The filtered dataframe is
# stored in simulationResults6_B.
simulationResults6_B = droplevels(simulationResults6[simulationResults6$NumberOfRows<16,])

# Density plots (on the filtered dataframe)
par(mfrow=c(1,2), mar=c(4.5,4.5,4,1))
densityPlot(PearsonSampletab ~ as.factor(NumberOfRows), data=simulationResults6_B,
    ylim=c(0.0, 0.4),
    col=myColours, lty=myLines, rug=FALSE, legend=FALSE,
    main='Pearson\'s Chi-square', xlab='Statistic')
densityPlot(LRatioSampletab ~ as.factor(NumberOfRows), data=simulationResults6_B,
    ylim=c(0.0, 0.4),
    col=myColours, lty=myLines, rug=FALSE,
    legend=list(title='Number of categories'),
    main='Likelihood Ratio', xlab='Statistic')


