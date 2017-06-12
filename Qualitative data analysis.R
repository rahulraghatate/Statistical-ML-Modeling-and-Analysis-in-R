#R basics  for Qualitative data
library(MASS)   # for painters dataset
school = painters$School
school.freq = table(school) #table function
school.freq
cbind(school.freq) # columnar type data print
school.relfreq = school.freq / nrow(painters) # nrows function
school.relfreq 

old = options(digits=1)  # options to set digit to 1
school.relfreq
options(old)  #restore options

barplot(school.freq)         # apply the barplot function

# for enhancement
colors = c("red", "yellow", "green", "violet","orange", "blue", "pink", "cyan") 
barplot(school.freq,col=colors) # apply the barplot function and set the color palette

pie(school.freq,col=colors)              # apply the pie function


#mean composition score of school C in the data set painters

c_school = school == "C"      # the logical index vector
c_painters = painters[c_school, ]  # child data set
mean(c_painters$Composition) 

#OR
tapply(painters$Composition, painters$School, mean) 

#the school with the highest composition scores.
tapply(painters$Composition, painters$School, max)

#percentage of painters whose color score is equal to or above 14.
Colour=painters$Colour
col_14 = Colour > 14
col_14_painters = painters[col_14,]
print(nrow(col_14_painters)*100/nrow(painters))



