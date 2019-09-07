# Summary statistics and information about the data source

path_transformed <- "data/2Transform.rda"
path_extracted <- "data/1Extract.rda"
load(path_transformed)
load(path_extracted)

# Variables for articles data frame and factors of each variable
names(df)
levels(df$year)
levels(df$month)
levels(df$publication)
levels(df$category)
levels(df$digital)

# Plot of 