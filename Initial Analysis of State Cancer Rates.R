library(tidyverse)
library(reshape2)
library(plotly)

# pull in the data and clean it up

d <- as.data.frame(read.table("State Death Rates.txt", header=T, fill=T))

names <- names(d)
names <- names[-1]
names(d) <- names

d <- d %>% 
  select(
    State,
    Year,
    Adjusted,
    Crude) %>%
  filter(
    State != "Total"
  )

d <- na.omit(d)

d$Year <- as.character(d$Year)
d$Year <- as.numeric(d$Year)

#  Add in Ranks
d$Crude_Rank <- ave(d$Crude, d$Year, FUN = function(x) rank(x, ties.method = "first"))
d$Adjusted_Rank <- ave(d$Adjusted, d$Year, FUN = function(x) rank(x, ties.method = "first"))

# look at ranks in 2015
d %>%
#  filter(Year=="2015") %>%
  mutate(new_rank = 52 -Adjusted_Rank)%>%
  select(
    State,
    Adjusted,
    Adjusted_Rank,
    new_rank
  ) 
 
# Create a line graph

lg <- ggplot(d, aes(x=Year, y=Adjusted, group=State)) +
  geom_line(alpha=.25) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # axis.line = element_line(colour = "grey")
    axis.line = element_blank(),
    text = element_text(size=18)
  ) +
  geom_line(data=subset(d,State=="Ohio"), aes(x=Year, y=Adjusted, group=State),
            color="#0072B2", size = 2) +
  scale_x_continuous(name="", breaks = c(2000,2005,2010,2015)) +
  ylab("")
ggplotly(lg)

# slope graph

e <- subset(d, Year %in% c(2000,2015))

ggplot(e, aes(x=Year, y=Adjusted, group=State)) +
  geom_line(alpha=.25) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
   # axis.line = element_line(colour = "grey")
   axis.line = element_blank()
  ) +
  geom_line(data=subset(e,State=="Ohio"), aes(x=Year, y=Adjusted, group=State),
            color="#0072B2", size = 2) +
  scale_x_continuous(name="Year", breaks = c(2000,2005,2010,2015))

# scatter plot  - not particularly interesting
f <- reshape2::dcast(d, State ~ Year, value.var = "Adjusted_Rank")

ggplot(data=f, aes(x=`2000`, y = `2015`)) +
  geom_point() +
  geom_point(data=subset(f,State=="Ohio"), aes(x=`2000`, y = `2015`), color= "#0072B2", size =2)

# consider all years an ob
d$isOhio <- as.factor(ifelse(d$State=="Ohio","Yes","No"))
ggplot(d, aes(x=Crude_Rank, y=Adjusted_Rank, color=isOhio)) +
  geom_point()

# Write the Table
write.csv(d, "State Cancer Mortality - Cleaned.csv", row.names = F)


# which is the greater influence, state or time
d <-as.data.frame(d[,1:6])

# Which state has been near the median value the most often
d %>%
  filter(between(Adjusted_Rank, 25,26) ) %>%
  mutate(new_rank = 52 -Adjusted_Rank)%>%
  select(
    State,
    Adjusted,
    Adjusted_Rank,
    new_rank
  ) %>%
  group_by(State) %>%
  summarise(
    n=n()
  ) %>%
  arrange(desc(n)) %>%
  head()

# Relevel so that Georgia is the reference state
d$State <- relevel(d$State,"Georgia")
d$States <- factor(d$States, levels = states )

# run regression/ANOVA
mod1 <- lm(data = d, Adjusted ~ Year + State)
summary(mod1)
summary.aov(mod1)
