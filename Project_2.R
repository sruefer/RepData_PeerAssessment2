# Code for PA_2.Rmd - "US Weather Events and their Consequences on Population Health and Economics"

## Synopsis

# as per report - not included here.


## Data Processing
## -------------------------------------------------------------------------------------------------

# Loading Libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(lubridate)

# Loading Data into R
x.raw <- read.csv("Data/repdata-data-StormData.csv.bz2")

# Subset dataset to relevant columns for population health investigation:
x <- select(x.raw,                                      # Data input from CSV
            BGN_DATE,                                   # Date of event
            EVTYPE,                                     # Event Type
            FATALITIES, INJURIES,                       # Number of fatalities and injuries
            PROPDMG, CROPDMG)                           # Economic damages

# Add year-only column and remove BGN_DATE
x$BGN_DATE <- mdy_hms(as.character(x$BGN_DATE))  # Convert date to POSIX
x <- mutate(x, YEAR = year(BGN_DATE))            # Add new column: YEAR (YYYY format)
x$BGN_DATE <- NULL                               # Remove original date column

# Look only at recent 15 years of data
x <- x %>% filter(YEAR > 1996)

# Add total impact for population and economy
x <- x %>%
     mutate(total_pop = INJURIES + FATALITIES, total_eco = PROPDMG + CROPDMG)

# Add 5-year time periods
periods <- c("1997 - 2001", "2002 - 2006", "2007 - 2011")
x$period <- periods[1]
x$period[x$YEAR >= 2002 & x$YEAR <= 2006] <- periods[2]
x$period[x$YEAR >= 2007] <- periods[3]
x$period <- factor(x$period)


### Population Consequences
### ------------------------------------------------------------------------------------------------

# Split into 3 time periods, and merge back together
pop1 <- data.frame()
for (p in periods) {
      # Create data frame from x dataset
      df <- x %>%
            filter(period == p) %>%
            group_by(EVTYPE) %>%
            summarise(fat_all = sum(FATALITIES), inj_all = sum(INJURIES), totals = sum(total_pop)) %>%
            filter(totals > 0) %>%
            mutate(period = p)
      
      # Combine / Merge
      if (length(pop1) == 0) {
            pop1 <- df
      } else {
            pop1 <- rbind(pop1, df)
      }
}
# Filter further and convert periods to factor variable
pop1 <- pop1 %>% filter(fat_all > 100, inj_all > 100)
pop1$period <- factor(pop1$period)

# Generate Facet Plot
p1 <- ggplot(as.data.frame(pop1), aes(x = inj_all, y = fat_all, size = totals)) + 
      geom_point(color = "red", alpha = 0.4) +
      facet_grid(. ~ period) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle("Fatalities vs Injuries over 5-year Periods") +
      xlab("Number of Injured") +
      ylab("Number of Fatalities") +
      theme_bw() +
      geom_text_repel(data = filter(pop1, totals >= 100), aes(label = EVTYPE))

# Generate data for consolidated Plot
pop2 <- x %>%
      group_by(EVTYPE) %>%
      summarise(fat_all = sum(FATALITIES), inj_all = sum(INJURIES), totals = sum(total_pop)) %>%
      filter(fat_all > 100, inj_all > 100)

# Generate consolidated bubble chart
p2 <- ggplot(as.data.frame(pop2), aes(x = inj_all, y = fat_all, size = totals)) + 
      geom_point(color = "red", alpha = 0.4) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle("Total Fatalities vs Injuries from 1997 - 2011") +
      xlab("Number of Injured") +
      ylab("Number of Fatalities") +
      theme_bw() +
      geom_text_repel(data = filter(pop2, totals >= 100), aes(label = EVTYPE))

# Combine into one Figure
p3 <- grid.arrange(p2, p1, nrow = 2)
p3

# Table of consolidated results
pop2 <- pop2 %>% arrange(desc(totals))
pop2[1:10,]


### Economic Consequences
### ------------------------------------------------------------------------------------------------

# Summarize and arrange the economic data
eco1 <- x %>% 
      group_by(EVTYPE) %>%
      summarise(crop_all = sum(CROPDMG), prop_all = sum(PROPDMG), totals = sum(total_eco)) %>%
      arrange(desc(totals))
      
# Display top 20 events that cause most economic damage
eco1[1:20,]

# Create Graph / Plot - Total Damage
p4 <- ggplot(eco1[1:10,], aes(x = reorder(EVTYPE, -totals), y = totals, fill = "Total")) +
      geom_bar(stat = "identity", alpha = 0.5) +
      geom_bar(stat = "identity", aes(x = reorder(EVTYPE, -totals), y = prop_all, fill = "Property")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90, vjust=0.5)) + 
      ggtitle("Total and Property Damage by Event Type from 1997 - 2011") +
      xlab("Event Type") +
      ylab("Damage Amounts") +
      scale_y_continuous(labels = abbreviate) +
      scale_fill_discrete(name="", guide = guide_legend(reverse = TRUE))
p4


## Results
## -------------------------------------------------------------------------------------------------


