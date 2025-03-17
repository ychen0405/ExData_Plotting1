## Exploratory data analysis assignment 1

library(data.table)
library(dplyr)
library(lubridate)

# Read the data using fread()
data <- fread("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

# Convert Date column to proper Date format (from DD/MM/YYYY)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Filter only the required dates
filtered_data <- data %>%
  filter(Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02")) %>%
  mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))

# Check structure
str(filtered_data)

# Plot 1: frequency of the global active power
hist(filtered_data$Global_active_power,
     col = "red",
     border = "black",
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency")
# Save plot 1
dev.copy(png, file = "Plot1.png")

# Plot 2: line chart with weekdays in x-axis and global active power in y-axis
# Extract weekday names
filtered_data$Weekday <- weekdays(filtered_data$DateTime, abbreviate = TRUE)

# Plot Global Active Power over time
plot(filtered_data$DateTime, filtered_data$Global_active_power, 
     type = "l", col = "black", lwd = 1,
     xlab = "", ylab = "Global Active Power (kilowatts)", xaxt = "n")

# Get the existing weekdays from data
unique_weekdays <- unique(filtered_data$Weekday)

# Manually extend to include "Sat"
extended_labels <- c(unique_weekdays, "Sat")  # Add "Sat" at the end
extended_positions <- seq(min(filtered_data$DateTime), 
                          max(filtered_data$DateTime) + (24*3600),  # Add 1 day
                          by = "day")

# Add custom x-axis with weekday labels including "Sat"
axis(1, at = extended_positions, labels = extended_labels)
# Save plot 2
dev.copy(png, file = "Plot2.png")

# Plot 3: multi-line time series plot that represents energy sub-metering over time for three different sub-meters
# Convert sub-metering data to numeric
filtered_data$Sub_metering_1 <- as.numeric(filtered_data$Sub_metering_1)
filtered_data$Sub_metering_2 <- as.numeric(filtered_data$Sub_metering_2)
filtered_data$Sub_metering_3 <- as.numeric(filtered_data$Sub_metering_3)

# Create the base plot (Sub_metering_1 in black)
plot(filtered_data$DateTime, filtered_data$Sub_metering_1, 
     type = "l", col = "black", lwd = 1,
     xlab = "", ylab = "Energy sub metering", xaxt = "n")
# Add Sub_metering_2 (red line)
lines(filtered_data$DateTime, filtered_data$Sub_metering_2, col = "red", lwd = 1)

# Add Sub_metering_3 (blue line)
lines(filtered_data$DateTime, filtered_data$Sub_metering_3, col = "blue", lwd = 1)

# Add custom x-axis with weekday labels including "Sat"
axis(1, at = seq(min(filtered_data$DateTime), max(filtered_data$DateTime) + (24*3600), by = "day"),
     labels = c("Thu", "Fri", "Sat"))

# Adjust Legend Position (Manually Set Coordinates)
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"), lwd = 1, 
       bg = "white")  
# Save plot 3
dev.copy(png, file = "Plot3.png")

# Plot 4
# Convert necessary columns to numeric
filtered_data$Global_active_power <- as.numeric(filtered_data$Global_active_power)
filtered_data$Voltage <- as.numeric(filtered_data$Voltage)
filtered_data$Global_reactive_power <- as.numeric(filtered_data$Global_reactive_power)

# Set up 2x2 layout
par(mfrow = c(2, 2)) 

# Top left
plot(filtered_data$DateTime, filtered_data$Global_active_power, 
     type = "l", col = "black", lwd = 1,
     xlab = "", ylab = "Global Active Power",
     xaxt = "n")
axis(1, at = seq(min(filtered_data$DateTime), max(filtered_data$DateTime) + (24*3600), by = "day"),
     labels = c("Thu", "Fri", "Sat"))

#Top right
plot(filtered_data$DateTime, filtered_data$Voltage, 
     type = "l", col = "black", lwd = 1,
     xlab = "datetime", ylab = "Voltage",
     xaxt = "n")
axis(1, at = seq(min(filtered_data$DateTime), max(filtered_data$DateTime) + (24*3600), by = "day"),
     labels = c("Thu", "Fri", "Sat"))

#Bottom left
plot(filtered_data$DateTime, filtered_data$Sub_metering_1, 
     type = "l", col = "black", lwd = 1,
     xlab = "", ylab = "Energy sub metering", xaxt = "n")
lines(filtered_data$DateTime, filtered_data$Sub_metering_2, col = "red", lwd = 1)
lines(filtered_data$DateTime, filtered_data$Sub_metering_3, col = "blue", lwd = 1)
axis(1, at = seq(min(filtered_data$DateTime), max(filtered_data$DateTime) + (24*3600), by = "day"),
     labels = c("Thu", "Fri", "Sat"))
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"), lwd = 1, bty = "n")

# Bottom right
plot(filtered_data$DateTime, filtered_data$Global_reactive_power, 
     type = "l", col = "black", lwd = 1,
     xlab = "datetime", ylab = "Global Reactive Power",
     xaxt = "n")
axis(1, at = seq(min(filtered_data$DateTime), max(filtered_data$DateTime) + (24*3600), by = "day"),
     labels = c("Thu", "Fri", "Sat"))

# save plot4
dev.copy(png, file = "Plot4.png")