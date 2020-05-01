getwd()
setwd("/Users/Henry/Desktop/Final/Social\ Media\ Analytics/Project3/data")
getwd()
car_data = read.csv('Everything.csv', header=TRUE)

attach(car_data)


cor(car_data[,c('afternoon','morning','early.morning','night')])
str(car_data)
g = lm( shares~ link+music+photo+question+swf+video)
summary(g)
g1 = lm( C2B_likes~ link+music+photo+question+swf+video)
summary(g1)
g2 = lm( total_comments~ link+music+photo+question+swf+video)
summary(g2)


