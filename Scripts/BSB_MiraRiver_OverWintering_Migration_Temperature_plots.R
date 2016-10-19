#2016-10-19 Colin F Buhariwalla
# OVERWINTERING - plot departure/arrival times with temperature across years 
# last updated:

# This script is meant to plot the overwintering departure/return events from Albert bridge into the OW area + Leaving overwintering area
# I need: 1) Temperature at BSB MR07 from Hobo Temp Logger (include relevant metadata in the script for easy reference)
#         2) a file with albert bridge departure events, arrival in OW events, and first out events 
#         3) Need to combine this event file with temperature file (take the temperature at the closest time stamp and give it to the departure event)

# Want to Produce: a plot with date on the X axis, temperature on the y axis, have events (leaving ab, arrival in ow, exit of ow) as different symbols
#                  along the temperature line, by year? 

# I need to decide on: 
# 1) What temperature to use (mean daily?, min, max)

#Steps:
# 1) Use my OWmigration file to produce the required information
# 2) Export this into a saved data file 
# 3) read in my water temperature file - ensure that it is cleaned up.
# 4) figure out how to fill in the temperature for each fish at the time of it's departure

####