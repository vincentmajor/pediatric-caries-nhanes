## Vincent Major
## Health Informatics
## Oct 17 2016

if(!require(RNHANES)){install.packages('RNHANES');library(RNHANES)}

#### From github readme.md ----

# Download environmental phenols & parabens data from the 2011-2012 survey cycle
dat <- nhanes_load_data("EPH", "2011-2012")

# Download the same data, but this time include demographics data (which includes sample weights)
dat <- nhanes_load_data("EPH", "2011-2012", demographics = TRUE)

# Find the sample size for urinary triclosan
nhanes_sample_size(dat,
                   column = "URXTRS",
                   comment_column = "URDTRSLC",
                   weights_column = "WTSA2YR")

# Compute the detection frequency of urinary triclosan
nhanes_detection_frequency(dat,
                           column = "URXTRS",
                           comment_column = "URDTRSLC",
                           weights_column = "WTSA2YR")

# Compute 95th and 99th quantiles for urinary triclosan
nhanes_quantile(dat,
                column = "URXTRS",
                comment_column = "URDTRSLC",
                weights_column = "WTSA2YR",
                quantiles = c(0.95, 0.99))

# Plot a histogram of the urinary triclosan distribution
nhanes_hist(dat,
            column = "URXTRS",
            comment_column = "URDTRSLC",
            weights_column = "WTSA2YR")

# Build a survey design object for use with survey package
design <- nhanes_survey_design(dat, weights_column = "WTSA2YR")

#### exploring oral health ----

# loading list of files and variables - because data updates regularly.
files <- nhanes_data_files()
variables <- nhanes_variables()

# search within files and variables. can also restrict searches
nhanes_search(files, "Oral Health")
nhanes_search(files, "pesticides", component == "laboratory", cycle == "2003-2004")
nhanes_search(files, "", cycle == "2003-2004")

nhanes_search(variables, "triclosan")
nhanes_search(variables, "DDT", data_file_name == "LAB28POC")
nhanes_search(variables, "", data_file_name == "EPH_E")

## Search for NHANES files related to environmental phenols and download all of them
file_results = nhanes_search(files, "Oral Health")
oral_data = nhanes_load_data(file_results$data_file_name, file_results$cycle)

oral_data.df = data.frame(oral_data[[31]])

## Selecting OHXPERIO to deep-dive regarding ids
ohxperio.df = nhanes_load_data('OHXPERIO', year = nhanes_search(files, "OHXPERIO")$cycle, demographics = TRUE)

files_99_00 = nhanes_search(files, "", cycle == "1999-2000")
drxiff = nhanes_load_data('DRXIFF', year = "1999-2000") ## rediculously detailed table of dietary intake. confused as to the meaning of each row.
cvx = nhanes_load_data('CVX', year = "1999-2000")
demo = nhanes_load_data('DEMO', year = "1999-2000") 
