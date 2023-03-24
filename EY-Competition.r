
##INFLATION RATE RECORDS
  
is.numeric(Inflation_records[2])

Year <- as.numeric(unlist(Inflation_records[1]))

Inflation <- as.numeric(unlist(Inflation_records[2]))

Distribution <- as.numeric(unlist(Inflation_records[3]))

Inf_records <- data.frame(Year = Year, Rate  = Inflation,
           Distribution = Distribution)

G1 <- ggplot(data = Inf_records, 
       aes(x = Rate, y  = Distribution)) +
  geom_point(size=2) +
  geom_smooth()

G2 <- ggplot(data = Inf_records, aes(x= Rate)) +
  geom_boxplot()

Fig1 <- ggarrange(G1, G2, nrow = 2, ncol = 1)

annotate_figure(Fig1,
                top = text_grob("Distribution of Inflation rate records from 1960 to 2021", 
                                color = "red", 
                                face = "bold", size = 14),
                bottom = text_grob("Data source: \n Macrotrends", 
                                   color = "blue",
                   hjust = 1, x = 1, face = "italic", size = 5))


##CIPR VS PENSION
Comparison <- read_excel("CIPR vs Pension.xlsx")

CIPR <- as.numeric(unlist(Comparison[1]))

Pension <- as.numeric(unlist(Comparison[2]))

Income <- data.frame(CIPR = CIPR, Pension = Pension)

Inc <- cbind(Income, simulation = 1:1000) %>%
  pivot_longer(-simulation, names_to = 'Product', values_to = 'Income')

ggplot(Inc, aes(x = simulation, y = Income, colour = Product)) +
  geom_line() + 
  labs(title = 'CIPR vs Pension Income')


## LIFE EXPECTANCY

Life <- Life_expectancy

Yr <- as.numeric(unlist(Life[1]))

Expectancy <- as.numeric(unlist(Life[2]))


LEtable <- data.frame(Year = Yr, Expectancy = Expectancy)

ggplot(data = LEtable, aes(x = Year, y = Expectancy)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = 'lm', size = 0.4, color = "red") +
  labs(title = 'Regression line of Life Expectancy over Year from 2014 - 2023',
       subtitle = 'R-squared: 0.9662')
  

summary(lm(LEtable$Expectancy ~ LEtable$Year))
##R^2 = 0.9662 ~ 96.62% 
##i.e., safe to say that life expectancy increases at a constant rate




##EMPLOYEE AVERAGE INCOME
Yr1 <- as.numeric(unlist(Earnings[1]))

Employee <- as.numeric(unlist(Earnings[2]))

Earn <- data.frame(Year = Yr1, Income = Employee)

ggplot(data = Earn, aes(x = Year, y = Employee)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = 'lm', size = 0.4, color = "red") +
  labs(title = 'Yearly Employee Earnings 1975 - 2022',
       subtitle = 'R-squared = 0.9829')


summary(lm(Earn$Income ~ Earn$Year))

##ACCUMULATED VALUE
Accumulated <- as.numeric(unlist(Book4))
Acc <- data.frame(Accumulated = Accumulated)

ggplot(data = Acc, aes(x = Accumulated)) +
  geom_boxplot()

summary(Book4)


##INTEREST RATE RECORDS

Years <- as.numeric(unlist(Interest_rate[1]))

interest <- as.numeric(unlist(Interest_rate[2]))

Distrib <- as.numeric(unlist(Interest_rate[3]))

Int_records <- data.frame(Year = Years, Rate  = interest,
                          Distribution = Distrib)

G3 <- ggplot(data = Int_records, 
             aes(x = Rate, y  = Distribution)) +
  geom_point(size=2) +
  geom_smooth()

G4 <- ggplot(data = Int_records, aes(x= Rate)) +
  geom_boxplot()

Fig2 <- ggarrange(G3, G4, nrow = 2, ncol = 1)

annotate_figure(Fig2,
                top = text_grob("Distribution of Interest rate records from 1993 to 2022", 
                                color = "red", 
                                face = "bold", size = 14),
                bottom = text_grob("Data source: \n Macrotrends", 
                                   color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 5))
