
library(ggplot2)
library(tidyverse)
library(ggthemes)


install.packages('hrbrthemes')

# read in CSV ( File path should have "/" slashes)
cah = read.csv('C:/Users/afisher/Documents/R Code/Resources/Data/Pulse of the Nation/201710cah_clean.csv')

# dim
dim(cah) # number of rows, number of columns

# glimpse
glimpse(cah) #observations, variables, and variable type

# head
head(cah) # first 6 observations

# Names of columns
names(cah)

# Trump Approval
table(cah$ApproveTrump)
barplot(table(cah$ApproveTrump))
barplot(table(cah$ApproveTrump), width=.75, col="steelblue", border="gray", ylim=c(0, 700))


# Trump or Vader
table(cah$VaderOrTrump)
pie(table(cah$VaderOrTrump))

# Education
table(cah$education)
cah$educ_numeric = as.numeric(cah$education)
table(cah$educ_numeric, cah$education)

library(politicaldata)
data("pres_results")
pa = pres_results %>% filter(state=='PA')
# Old Fashion line plot
plot(pa$year, pa$rep, ylab="% Vote for President", xlab="Year", lwd=3, col="red", type="b", pch=16, ylim=c(0,1))
par(new=T)
lines(pa$year, pa$dem,lwd=3, col="blue", type="b", pch=16)
legend(2005, .9)

# ggplot
ggplot(pa)+
  geom_line(aes(x=year, y=dem), color='blue') +
  geom_line(aes(x=year, y=rep), color='red') +
  xlab('Year') +
  ylab('% Vote') +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1976,2016,4)) +
  theme_economist()

       
# After Melting
pa_long <- gather(pa, party, vote, dem:other, factor_key=TRUE)
ggplot(pa_long)+
  aes(x=year, y=vote, colour=party)+
  geom_line(stat='identity')+
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1976,2016,4)) +
  ggtitle("Vote Percentage by Party") + 
  labs(x="Year", y="Vote %") +
  theme_fivethirtyeight()












