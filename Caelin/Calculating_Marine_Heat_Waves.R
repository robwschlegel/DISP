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

#MHWs each year
Year_2006 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2006-01-01",
           end_date = "2007-01-01", category = TRUE, spread = 150)
Year_2007<- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2007-01-01",
           end_date = "2008-01-01", category = TRUE, spread = 150)
Year_2008 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2008-01-01",
           end_date = "2009-01-01", category = TRUE, spread = 150)
Year_2009 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2009-01-01",
           end_date = "2010-01-01", category = TRUE, spread = 150)
Year_2010 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2010-01-01",
           end_date = "2011-01-01", category = TRUE, spread = 150)
Year_2011 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2011-01-01",
           end_date = "2012-01-01", category = TRUE, spread = 150)
Year_2012 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2012-01-01",
           end_date = "2013-01-01", category = TRUE, spread = 150)
Year_2013 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2013-01-01",
           end_date = "2014-01-01", category = TRUE, spread = 150)
Year_2014 <- event_line(data = Event_Data, metric = "intensity_max_relThresh", start_date = "2014-01-01",
           end_date = "2015-01-01", category = TRUE, spread = 150)



