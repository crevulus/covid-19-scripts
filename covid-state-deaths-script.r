library(readr)
library(deplyr)
library(purrr)
library(viridis)
library(usmap)

data <- read.csv("./us-states.csv/")
pop_data <- read.csv("./nst-est2019-alldata.csv/")

# Show case:death ratio by state
death_ratio_state_data <- data%>%
  group_by(state)%>%
  summarize(
    sum_cases=sum(cases),
    sum_deaths=sum(deaths)
  )

death_ratio_state_plot <- ggplot(data=death_ratio_state_data,
  aes(x=sum_cases, y=sum_deaths, color=state)) +
  geom_point() +
  # Only show label if over 1m cases; position label
  geom_text(aes(label=ifelse(sum_cases>10000000,as.character(state),'')),hjust=0.5,vjust=1) +
  theme(legend.position = "none") +
  # Set axes to show standard numbers, not formulaic numbers; Set axes limits
  scale_y_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, 3000000)) +
  scale_x_continuous(name="Deaths", labels = scales::comma, expand = c(0, 0), limits = c(0, 40000000))

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
    sum_cases=sum(cases),
    sum_deaths=sum(deaths)
  )

# Bar
deaths_per_day_bar <- ggplot(data=deaths_per_day_data,
  aes(x=day_of_the_week, y=sum_deaths)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, NA))

deaths_per_day_bar

# Line
death_rate_data <- deaths_per_day_data%>%
  mutate(
    deaths_cases_percent = sum_deaths/sum_cases * 100
  )

death_rate_line <- ggplot(data=death_rate_data,
  aes(x=day_of_the_week, y=deaths_cases_percent)) +
  geom_line(size = 1.5, color="red", group=1)

death_rate_line

# Combined
combined_chart <- ggplot(death_rate_data, aes(x=day_of_the_week)) +
  geom_bar(aes(y=sum_deaths), color = "darkblue", fill = "lightblue", stat="identity") +
  # multiply by 200k to bring up to same scale
  geom_line(aes(y=200000*deaths_cases_percent), size = 1.5, color="red", group=1) +
  # divide secondary y axis by 200k to be accurate with line data
  scale_y_continuous(sec.axis = sec_axis(~./200000, name="Deaths per 100 Cases"), expand = c(0, 0), limits = c(0, NA))




# Show coloured map of cases
cases_fips_data <- data%>%
  group_by(fips)%>%
  summarize(
    sum_cases=sum(cases),
    sum_deaths=sum(deaths)
  )

state_map <- us_map(regions = "states")

cases_map <- plot_usmap("states", data = cases_fips_data, value="sum_cases") +
  scale_fill_continuous(name="Cases by State", low = "#FDEDEC", high = "red", guide = FALSE) +
  theme(legend.position = "right")


death_ratio_state_plot
cases_map
combined_chart
