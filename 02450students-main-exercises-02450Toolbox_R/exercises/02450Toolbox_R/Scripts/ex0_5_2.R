####################
# Exercise 0.5.2
####################

# We simulate measurements every 100 ms for a period of 10 seconds
t <- seq(from = 0, by = 0.1, to = 10)

# The data from the sensors are generated as either a sine or a cosine
# with some Gaussian noise added.
sensor1 <- 3 * sin(t) + 0.5 * rnorm(length(t))
sensor2 <- 3 * cos(t) + 0.5 * rnorm(length(t))

# Change the font size to make axis and title readable
# font_size = 15
# plt.rcParams.update({'font.size': font_size})

# Define the name of the curves
legend_strings <- c("Sensor 1", "Sensor 2")

# Start plotting the simulated measurements
# Plot the sensor 1 output as a function of time, and
# make the curve red and fully drawn
plot(t, sensor1, # plot sensor1 as a function of t
  type = "l", lty = 1, # set linetype
  col = "red", # define plot colour
  ann = FALSE
) # remove variable name label annotation

# Plot the sensor 2 output as a function of time, and
# make the curve blue and dashed
lines(t, sensor2, type = "l", lty = 2, col = "blue", ann = FALSE)

# Add a grid in the background
grid()

# Add a legend describing each curve, place it at the "best" location
# so as to minimize the amount of curve it covers
legend(1, 1, legend_strings, col = c("red", "blue"), lty = 1:2)

# Add labels to the axes and a title to the plot
title(
  main = "Sensor outputs",
  xlab = "Time [s]",
  ylab = "Voltage [mV]"
)

# Export the figure
# dev.copy(png, 'ex0_5_2.png')
# dev.off()
