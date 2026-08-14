# Samples specific subsets based on rows, columns, and optionally slices
# from a data frame.
#
# This function allows for random sampling of rows and columns from a
# given dataframe, with an option to also sample by another dimension,
# referred to as 'slices' here. It's useful for creating smaller,
# manageable subsets from large datasets for analysis or testing. The
# function ensures that the sampled subsets are representative by
# randomly selecting specified numbers of unique rows, columns, and
# slices.
#
# Args:
#   inputDF: The dataframe from which to sample. It must contain columns
#            named 'Row', 'Column', and optionally 'Slice' if noSlis is
#            not NULL.
#   noRows:  The number of unique rows to sample.
#   noCols:  The number of unique columns to sample.
#   noSlis:  The number of unique slices to sample (optional);
#            i.e., if NULL, slicing is not performed.
#
# Returns:
#   A subset of the input dataframe, filtered based on the sampled rows,
#   columns, and optionally slices. The returned dataframe will have
#   unused factor levels dropped to keep the factor variables tidy.
#
# Example usage:
#   subDF <- sample_data_subset(df, noRows=10, noCols=5, noSlis=2)
#   This would return a dataframe sampled randomly with 10 rows, 5
#   columns, and 2 slices.
sample_data_subset <- function(inputDF, noRows, noCols, noSlis=NULL) {
    # Sample the specified number of unique row and column identifiers
    # from the dataframe.
    sampleRows <- sample(unique(inputDF$Row), noRows)
    sampleCols <- sample(unique(inputDF$Column), noCols)

    # Filter the data frame based on the sampled row and column
    # identifiers. This creates a new dataframe (subDF) that only
    # includes the rows and columns selected.
    subDF <- inputDF[(inputDF$Row %in% sampleRows) & (inputDF$Column %in% sampleCols),]

    # Check if slice sampling is required based on whether noSlis is
    # provided.
    if (!is.null(noSlis)) {
        # Sample the specified number of unique slices if noSlis is not NULL.
        sampleSlis <- sample(unique(inputDF$Slice), noSlis)
        
        # Further filter the already filtered dataframe (subDF) by the
        # sampled slices.
        # This step is skipped if no slices are to be sampled.
        subDF <- subDF[subDF$Slice %in% sampleSlis, ]
    }

    # Return the final subset dataframe with unused factor levels dropped.
    # Dropping unused levels cleans up the factor variables, making the
    # data tidier for analysis.
    return(droplevels(subDF))
} # END OF sample_data_subset()



# Filters a dataframe based on a dynamically selected subset of values
# from a specified column.
#
# This function first selects a random subset of the specified values
# (if a subset size is provided), then filters the dataframe to include
# only rows where the column matches any of these randomly selected
# values. It's particularly useful for scenarios where analysis or
# sampling is needed on a representative subset of categories within a
# larger dataset.
#
# Args:
#   inputDF:      Data frame to filter.
#   filterColumn: The name of the column to filter by.
#   filterValues: A vector of values to potentially filter the column by.
#   subsetSize:   Optional. The number of values to randomly select from
#                 filterValues for filtering. If NULL or not specified,
#                 all values in filterValues are used for filtering.
#
# Returns:
#   A filtered dataframe with rows matching the dynamically chosen
#   subset of criteria.
#
# Example usage:
#   filtered_df <- filter_data_by_values(df, 'IndustryCode', cats, subsetSize=2)
#   To filter 'df' to include rows where 'IndustryCode' matches a random
#   selection of 2 categories from 'cats'.
filter_data_by_values <- function(inputDF, filterColumn, filterValues, subsetSize=NULL) {
    # Check if subsetSize is specified and less than the length of filterValues.
    if (!is.null(subsetSize) && subsetSize < length(filterValues)) {
        # Randomly select a subset of filterValues if subsetSize is specified.
        selectedValues <- sample(filterValues, size=subsetSize, replace=FALSE)
    } else {
        # Use all filterValues if subsetSize is not specified or greater
        # than the length of filterValues.
        selectedValues <- filterValues
    }
  
    # Apply filtering: Select rows where the column specified contains
    # values in selectedValues.
    # droplevels() is used to drop unused factor levels in the filtered dataframe.
    filteredDF <- droplevels(inputDF[inputDF[[filterColumn]] %in% selectedValues,])
  
    return(filteredDF)
} # END OF filter_data_by_values()



# Samples data from a dataframe based on a specified size and method
# (with or without replacement) and returns a crosstabulation
# (cross-tab) of the sampled data.
#
# This function expands the input dataframe by repeating rows according
# to their frequency (specified in the 'Freq' column), samples the
# specified number of rows from this expanded set (with or without
# replacement), and then creates a crosstabulation from the sampled
# data. It's particularly useful for creating contingency tables from
# sampled data for statistical analysis or for generating reports.
#
# Args:
#   inputDF:         Dataframe from which to sample. Must contain a 'Freq'
#                    column indicating the frequency of each row.
#   sampleSize:      The number of samples to draw from the expanded
#                    dataframe.
#   withReplacement: Logical indicating whether sampling should be with
#                    replacement (TRUE) or without replacement (FALSE).
#
# Returns:
#   A crosstabulation (xtabs object in R) of the sampled data, which can
#   be used for further analysis or reporting. The dimensions of the
#   crosstabulation depend on the remaining columns (other than 'Freq')
#   in the input dataframe.
#
# Example usage:
#   crosstab <- sample_data_to_crosstab(inputDF=myDataframe, sampleSize=100, withReplacement=TRUE)
#   This samples 100 rows (with replacement) from 'myDataframe'
#   according to their frequencies and returns a crosstab.
sample_data_to_crosstab <- function(inputDF, sampleSize, withReplacement) {
    # Expand the input dataframe by repeating each row according to its
    # 'Freq' value. This ensures that the sampling process takes into
    # account the frequency of occurrences.
    # The last column, assumed to be 'Freq', is excluded from the
    # expanded dataframe.
    expandedDF <- inputDF[rep(1:nrow(inputDF), inputDF$Freq), -ncol(inputDF), drop=FALSE]

    # Sample the specified number of rows from the expanded dataframe.
    # The 'replace' parameter determines if the sampling is with or
    # without replacement.
    sampledDF <- expandedDF[sample(nrow(expandedDF), size=sampleSize, replace=withReplacement),]

    # Construct a crosstabulation of the sampled data.
    # The formula dynamically includes all columns of the sampled
    # dataframe in the crosstabulation.
    formula <- as.formula(paste('~', paste(names(sampledDF), collapse=' + ')))

    # Return the crosstabulation (cross-tab) as an 'xtabs' object, which
    # can be used for further analysis.
    return(xtabs(formula, data=sampledDF))
} # END OF sample_data_to_crosstab()



