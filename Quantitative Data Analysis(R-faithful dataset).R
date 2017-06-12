#R basics for Quantitative data (continuous)
#dataset -- faithful

head(faithful)
nrow(faithful)
#frequency distribution of the eruption durations in faithful.
duration = faithful$eruptions 
range(duration) # idea about min and max value

breaks = seq(1.5, 5.5, by=0.5)    # half-integer sequence for non-overlapping sub-intervals

#Classify the eruption durations according to the half-unit-length sub-intervals with cut
duration.cut = cut(duration, breaks, right=FALSE)

duration.freq = table(duration.cut)
cbind(duration.freq) 

#relative frequency table
duration.relfreq = duration.freq / nrow(faithful)
duration.cumfreq = cumsum(duration.freq)
duration.cumrelfreq = duration.cumfreq / nrow(faithful)
old = options(digits=1) 
cbind(duration.freq,duration.relfreq,duration.cumfreq,duration.cumrelfreq)
options(old)
#OR
transform(table(duration.cut))
transform(duration.freq,Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))

#Histogram
hist(duration,right=FALSE)

#Cumulutive freq graph
cumfreq0 = c(0, cumsum(duration.freq))
plot(breaks, cumfreq0,main="Old Faithful Eruptions", xlab="Duration minutes",ylab="Cumulative eruptions")   # y???axis label 
lines(breaks, cumfreq0) # join the points

#the duration sub-interval that has the most eruptions.
max(duration.freq)
which.max(duration.freq)
duration.freq[which.max(duration.freq)]
#OR
subset(duration.freq, duration.freq == max(duration.freq)) #subset gives all rows instead of one


####FOr Waitning Period of Eruptions

#the frequency distribution of the eruption waiting periods in faithful.
waiting = faithful$waiting
head(waiting)
range(waiting)
bins = seq(40,102, by=8)
waiting.cut = cut(waiting,bins, right = FALSE)
waiting.freq = table(waiting.cut)
#relative frequency distribution of the eruption waiting periods in faithful.
waiting.relfreq = waiting.freq/nrow(faithful)
#the cumulative frequency distribution of the eruption waiting periods in faithful.
waiting.cumfreq = cumsum(waiting.freq)
#the cumulative frequency distribution of the eruption waiting periods in faithful.
waiting.cumrelfreq = waiting.cumfreq/nrow(faithful)
cbind(waiting.freq,waiting.relfreq,waiting.cumfreq,waiting.cumrelfreq)
#OR
transform(waiting.freq,Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq), Cum_Rel_Freq = cumsum(Freq)/nrow(faithful))

#Histogram
hist(waiting,right = FALSE)

#the cumulative frequency graph of the eruption waiting periods in faithful.

cumfreq1 = c(0, cumsum(waiting.freq))
plot(bins, cumfreq1,main="Old Faithful Eruptions", xlab="wait time in minutes",ylab="Cumulative wait time")  
lines(bins, cumfreq1) # join the points

#the cumulative relative frequency graph of the eruption waiting periods in faithful.
fun= ecdf(waiting)
plot(fun,main="Old Faithful Eruptions", xlab="wait time in minutes",ylab="Cumulative waiting period proportion")

#the stem-and-leaf plot of the eruption waiting periods in faithful.
stem(waiting)


#Scatter Plot to predict relationship

plot(duration,waiting,xlab="Eruption duration",ylab="time waited")
#Relationship 
abline(lm(waiting ~ duration))

#Data Statistics(Numerical Measures)
summary(faithful)

print(sd(duration),sd(waiting))
print(IQR(duration),IQR(waiting))

cov(duration,waiting) #how the two are linearly related.

#correlation coefficient = covariance divided by the product of their individual standard deviations.
cor(duration, waiting)          # apply the cor function

#The correlation coefficient of eruption duration and waiting time is 0.90081.
# As ~ 1, the variables are positively linearly related.

#Central Moment
#2nd CM is variance
#2nd and 4th central moment of eruption waiting period in the data set faithful.
moment(waiting, order=2, center=TRUE) 
moment(waiting, order=4, center=TRUE) 

library(e1071) # for following functions

#skewness - Measure of symmetry
#rightskewed - mean>median positive skewness
#leftskewed - mean<median neg skewness
skewness(duration)
skewness(waiting)


#the kurtosis describes the tail shape of the data distribution
#mesukurtic --> normal distribution - 0 kurtosis , standard tail shape
#platykurtic--> thin-tailed  dist - neg kurtosis
#leptokurtic --> fat-tailed dist - positive

kurtosis(duration) 
kurtosis(waiting)
