# John Hopkins Data Science Coursera "Exploratory Data Analysis"
# Submitted by Jim Stearns in satisfaction of course assignment.
# File: plot2.R
# Due Date: 7-Sep-2014

setwd("~/GoogleDrive/Learning/Courses/CourseraDataScience/4_ExploratoryDataAnalysis/ProjectAssignments/C4W1Project/ExData_Plotting1")

# Prepare data if file containing subset of interest has not already been created.
# - Read from zipped dataset
# - Only read records for timespan of interest 2007-02-01 to 2007-02-02 inclusive.
# - Create a datetime column using the separate date and time columns. Leave the separate columns
# - Look for records with missing ('?') data and replace with NA.
# But for a project assignment restricting solution to four files, one per plot, this function
# should be placed in its own R script file and sourced by the four plot scripts.
createEnergySubsetOfInterest <- function(ensoi.fn) {
    zipOfHouseholdPowerConsumptionData <- "exdata-data-household_power_consumption.zip"
    stopifnot(file.exists(zipOfHouseholdPowerConsumptionData))
    
    # Unzip into a temporary directory
    tempzipdir <- tempfile()
    dir.create(tempzipdir)
    unzip(zipOfHouseholdPowerConsumptionData, exdir=tempzipdir)
    
    # Side-note: tried to use package "sqldf" to only read in the lines for the time range 
    # of interest, but encountered error: SQLDF APPEARS TO REQUIRE X11 ON MAC
    
    # Characteristics of the data:
    
    # Header and first data row:
    # Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
    # 16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000
    
    # Separator is semi-colon, not comma.
    # Missing data indicator is '?'.
    # Date format is d/m/yyyy - first column. Day and month are not zero-padded to 2 digits.
    energyColClasses=c("character", "character", "numeric", "numeric", "numeric", 
                       "numeric", "numeric", "numeric", "numeric")
    # TODO: Use readlines() to filter input line by line rather than reading all into memory.
    # But I have enough memory, so this works ...
    enall.df <- read.csv(file.path(tempzipdir, "household_power_consumption.txt"),
                           sep=";", na.strings="?", colClasses=energyColClasses)
    
    # Subset to a few days ...
    
    # Just to be safe, specify dates both with and without zero pad in month and day.
    datesOfInterest = c('01/02/2007', '1/2/2007', '02/02/2007','2/2/2007')
    v <- is.element(enall.df$Date, datesOfInterest)
    ensoi.df <- enall.df[v,]
    summary(ensoi.df)
    
    # Combine the date and time information into a new DateTime column
    dates <- ensoi.df[,1]
    times <- ensoi.df[,2]
    # Create a POSIX calendar time date/time column. Avoid as.Date() - doesn't handle time.
    DateTime <- as.POSIXct(strptime(paste(dates, times), "%d/%m/%Y %H:%M:%S"))
    ensoi.dt.df <- cbind(DateTime, ensoi.df)
    write.csv(ensoi.dt.df, file=ensoi.fn)
}

readTheEnergySubsetOfInterestFile <- function(ensoi.fn) {
    # Two additional columns: a sequence number as first and a POSIXct date in second.
    # Don't read in the sequence column, but do read in the date/time as POSIXct.
    energySubsetOfInterestClasses=c(
        "NULL", "POSIXct",
        "character", "character", # date and time
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", "numeric"
    )
    ensoi.df = read.csv(ensoi.fn, colClasses=energySubsetOfInterestClasses)
    return(ensoi.df)
}

doThePlot <- function(df) {
    par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))
    with(df, plot(DateTime, Global_active_power, type="n", 
         xlab="", ylab="Global Active Power (kilowatts)"))
    with (df, lines(DateTime, Global_active_power, lwd=1))
}

# Read in the Energy Sub-set of interest, creating from the full set of data if not already done.
ensoi.fn = "hh_power_subset.csv"
if (!file.exists(ensoi.fn)) {
    createEnergySubsetOfInterest(ensoi.fn)
}
ensoi.df = readTheEnergySubsetOfInterestFile(ensoi.fn)

# Plot to a 480x480 png file.
png("plot2.png", width=480, height=480)
doThePlot(ensoi.df)
dev.off()

# And just for grins, a copy to the screen.
plot.new()
doThePlot(ensoi.df)

