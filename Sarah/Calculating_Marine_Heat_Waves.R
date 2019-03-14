#January 31st, 2019
#UNDERSTAND THIS

library(heatwaveR)


# Calculate Climatologies -------------------------------------------------

# 1) CREATE AN OBJECT

New_Climatology <- ts2clm(data = sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31"))  #TIME SERIES 2 CLIMATOLOGY...MEDITARANIAN... YEAR-MONTH-DAY

Event_Data <- detect_event(data = New_Climatology)

Event_Data_Only <- Event_Data$event



# Figures -----------------------------------------------------------------

event_line(data = Event_Data, metric = "intensity_max_relThresh", category = TRUE)

lolli_plot(Event_Data)

event_categories <- category(MHW_north, S = FALSE, name = "Nova_Scotia")
