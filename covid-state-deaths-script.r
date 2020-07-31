library(readr)
library(dplyr)
library(purrr)
library(usmap)
library(ggplot2)

data <- read.csv("us-states.csv")
pop_data <- read.csv("nst-est2019-alldata.csv")

filtered_pop_data <- select(pop_data,
  STATE, NAME, POPESTIMATE2019)%>%
  filter(
    STATE != 0
  )

head(filtered_pop_data)

# Show case:death ratio by state
death_ratio_state_data <- data%>%
  group_by(state)%>%
  summarize(
    cum_cases=max(cases),
    cum_deaths=max(deaths)
  )

death_ratio_state_plot <- ggplot(data=death_ratio_state_data,
  aes(x=cum_cases, y=cum_deaths, color=state)) +
  geom_point() +
  # Only show label if over 1m cases; position label
  geom_text(aes(label=ifelse(cum_cases>100000,as.character(state),'')),hjust=0.5,vjust=1) +
  theme(legend.position = "none") +
  # Set axes to show standard numbers, not formulaic numbers; Set axes limits
  scale_y_continuous(name="Deaths", labels = scales::comma, expand = c(0, 0), limits = c(0, 40000)) +
  scale_x_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, 500000)) +
  labs(title="Deaths & Cases Per State")


joined_table <- left_join(death_ratio_state_data, filtered_pop_data, by=c("state" = "NAME"), copy=TRUE)

death_ratio_state_pop_data <- joined_table%>%
  mutate(
    cases_percent = (cum_cases/POPESTIMATE2019)*100,
    deaths_percent = (cum_deaths/POPESTIMATE2019)*100,
  )

death_cases_ratio_state_plot <- ggplot(death_ratio_state_pop_data,
  aes(x=cases_percent, y=deaths_percent)) +
  geom_point() +
  geom_text(aes(label=ifelse((cases_percent>1.5|deaths_percent>0.05),as.character(state),'')),hjust=0.5,vjust=1) +
  labs(title="Deaths & Cases Percentages", x="Confirmed % of Pop with COVID-19", y="Confirmed % Pop Killed by COVID-19")

death_ratio_state_pop_plot <- ggplot(death_ratio_state_pop_data,
  aes(x=POPESTIMATE2019, y=deaths_percent)) +
  geom_point() +
  geom_text(aes(label=ifelse((deaths_percent>0.1|POPESTIMATE2019>10000000),as.character(state),'')),hjust=0.5,vjust=1) +
  scale_x_continuous(name="Population", labels = scales::comma, expand = c(0, 0)) +
  labs(title="Deaths & Cases as a Percentage of Population", y="Confirmed % Pop Killed by COVID-19")



# Isolate cluster
# Isolate timespan



# Show deaths per weekday
deaths_per_day_data <- data%>%
  mutate(
    # Convert YYYY-MM-DD to days of the week
    day_of_the_week = format(as.Date(data[["date"]]), "%A"),
    day_of_the_week = factor(day_of_the_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
  )%>%
  group_by(day_of_the_week)%>%
  summarize(
    cum_cases=max(cases),
    cum_deaths=max(deaths)
  )

# Bar
deaths_per_day_bar <- ggplot(data=deaths_per_day_data,
  aes(x=day_of_the_week, y=cum_deaths)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, NA))

deaths_per_day_bar

# Line
death_rate_data <- deaths_per_day_data%>%
  mutate(
    deaths_cases_percent = cum_deaths/cum_cases * 100
  )

death_rate_line <- ggplot(data=death_rate_data,
  aes(x=day_of_the_week, y=deaths_cases_percent)) +
  geom_line(size = 1.5, color="red", group=1)

death_rate_line

# Combined
combined_chart <- ggplot(death_rate_data, aes(x=day_of_the_week)) +
  geom_bar(aes(y=cum_deaths), color = "darkblue", fill = "lightblue", stat="identity") +
  # multiply by 200k to bring up to same scale
  geom_line(aes(y=2000*deaths_cases_percent), size = 1.5, color="red", group=1) +
  # divide secondary y axis by 200k to be accurate with line data
  scale_y_continuous(sec.axis = sec_axis(~./2000, name="Deaths per 100 Cases"), expand = c(0, 0), limits = c(0, NA))




# Show coloured map of cases
cases_fips_data <- data%>%
  group_by(fips)%>%
  summarize(
    cum_cases=max(cases),
    cum_deaths=max(deaths)
  )

state_map <- us_map(regions = "states")

cases_map <- plot_usmap("states", data = cases_fips_data, value="cum_cases") +
  scale_fill_continuous(name="Cases by State", low = "#FDEDEC", high = "red", guide = FALSE) +
  labs(title="Map of Cases")


joined_fips_table <- left_join(data, filtered_pop_data, by=c("state" = "NAME"), copy=TRUE)

joined_fips_data <- joined_fips_table%>%
  group_by(fips)%>%
  summarize(
    cum_cases=max(cases),
    cum_deaths=max(deaths),
    population=max(POPESTIMATE2019)
  )

joined_fips_data <- joined_fips_data%>%
  mutate(
    cases_percent=cum_cases/population*100,
    deaths_percent=cum_deaths/population*100
  )

pop_cases_map <- plot_usmap("states", data = joined_fips_data, value="cases_percent") +
  scale_fill_continuous(name="Cases by State", low = "#FDEDEC", high = "red", guide = FALSE) +
  labs(title="Map of Cases by Population")

pop_deaths_map <- plot_usmap("states", data = joined_fips_data, value="deaths_percent") +
  scale_fill_continuous(name="Cases by State", low = "#FDEDEC", high = "red", guide = FALSE) +
  labs(title="Map of Deaths by Population")



death_ratio_state_plot
cases_map
death_ratio_state_pop_plot
pop_deaths_map
death_cases_ratio_state_plot
pop_cases_map
combined_chart
