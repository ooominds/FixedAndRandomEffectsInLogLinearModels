
require(car) # various auxiliary functions including densityPlot

source('auxiliary_functions.R') # auxiliary sampling functions

set.seed(125) # to replicate results

# Read simulated frequency table
freqtbl = read.csv('../data/simulatedONS.csv')

# All categories are considered
evenCat = unique(freqtbl$IndustryCode)

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
# simulationResults5.2 = data.frame(
#     NumberOfRows = rep(NA, numberOfDraws),
#     NumberOfColumns = rep(NA, numberOfDraws),
#     PearsonSampletab = rep(NA, numberOfDraws),
#     LRatioSampletab = rep(NA, numberOfDraws)
# )
# 
# # Execute the simulation loop. For each draw, sample a subtable from the
# # dataset, create a crosstab from the sampled data, and calculate
# # statistical metrics. The size of the subset varies for each draw,
# # randomly chosen from the range of available categories. Results
# # include the dimensions of the crosstable and calculated Pearson's
# # Chi-squared and likelihood ratio statistics. They are stored in the
# # simulationResults5.2 dataframe.
# # NOTE: Warnings may occur when expected values are small and, therefore,
# # the approximation of the p-value may be incorrect. This can happen
# # when the sampleTableTotal is not sufficiently large. Uncomment the
# # code below to suppress the message.
# # suppressWarnings(
# for (i in 1:numberOfDraws) {
#     subsetSize = sample(2:length(evenCat), 1)
#     simtable = filter_data_by_values(inputDF=freqtbl, filterColumn='IndustryCode', filterValues=evenCat, subsetSize=subsetSize)
#     crosstable = sample_data_to_crosstab(inputDF=simtable, sampleSize=sampleTableTotal, withReplacement=TRUE)
#     crosstable = crosstable[rowSums(crosstable < 2) == 0,]
#     simulationResults5.2$NumberOfRows[i] = dim(crosstable)[1]
#     simulationResults5.2$NumberOfColumns[i] = dim(crosstable)[2]
#     simulationResults5.2$PearsonSampletab[i] = loglin(crosstable, dependencies, print=FALSE)$pearson
#     simulationResults5.2$LRatioSampletab[i] = loglin(crosstable, dependencies, print=FALSE)$lrt
#     cat(paste0(i, '\n')) # track progress
# }
# # )
# 
# # Save results
# save(simulationResults5.2, file='simulationResults5.2.rda')
# 
# ###################################################
# ### END OF THE PART TO BE RUN ON A SERVER / HPC ###
# ###################################################

### RESULTS ###

# Load results
load('../results/simulationResults5.2.rda')

# Number of tables by row sizes
table(simulationResults5.2$NumberOfRows)
#   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
# 537 496 539 492 534 514 550 525 557 529 543 539 491 508 526 538 551 546 485 

# Number of tables by column sizes
table(simulationResults5.2$NumberOfColumns)
#     2 
# 10000 

# Initialize a list to store the proportion of tables of different
# degrees of freedom where Pearson Chi-squared statistics exceed the
# critical value at 5%.
howMany = list()
for (cols in 2:20) {
    currDF = cols * 2 - (cols-1) - (2-1) - 1
    currChi05 = qchisq(0.05, df=currDF, lower.tail=FALSE)
    currGreater = prop.table(table(simulationResults5.2$PearsonSampletab[simulationResults5.2$NumberOfRows==cols] > currChi05))
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
# 0.039 0.046 0.030 0.039 0.062 0.039 0.044 0.067 0.052 0.025 
#    12    13    14    15    16    17    18    19    20 
# 0.053 0.063 0.039 0.030 0.055 0.043 0.051 0.044 0.033 

# Define colours and lines for plotting
myColours = c('#333333', '#ef3b2c', '#33a02c', '#6a3d9a', '#08519c',
    '#a65628', '#888888', '#b30000', '#b2df8a', '#ff7f00', '#cab2d6',
    '#377eb8', '#bdbdbd', '#225ea8', '#8c510a', '#54278f', '#80cdc1',
    '#252525', '#662506')
myLines = c('solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid',
    'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dashed', 'dotted',
    'dotted', 'dotted', 'dotted', 'dotted', 'dotted')

# Density plots
par(mfrow=c(1,2), mar=c(4.5,4.5,4,1), cex=0.9)
densityPlot(PearsonSampletab ~ as.factor(NumberOfRows), data=simulationResults5.2,
    ylim=c(0.0, 0.4),
    col=myColours, lty=myLines, rug=FALSE, legend=FALSE,
    main='Pearson\'s Chi-square', xlab='Statistic')
densityPlot(LRatioSampletab ~ as.factor(NumberOfRows), data=simulationResults5.2,
    ylim=c(0.0, 0.4),
    col=myColours, lty=myLines, rug=FALSE,
    legend=list(title='Number of categories'),
    main='Likelihood Ratio', xlab='Statistic')



