## Stock-recruit relationships

num.mom <- 1:5000
a = 1
b= 0.001
noise <- 0.1

# So here's a Ricker
rec <- a*num.mom*exp(-b*num.mom)*rlnorm(5000,0,noise)

# here's the fake data
dat <- data.frame(rec = rec, num.mom = num.mom)
# So here's what is looks like with all the information
ggplot(dat) + geom_point(aes(x=num.mom,y=rec))
ggplot(dat) + geom_point(aes(x=num.mom,y=rec/num.mom))
ggplot(dat) + geom_point(aes(x=num.mom,y=log(rec/num.mom)))

# But what if the stock has been only varied from 500 to 2000 individuals
size.sub <- 500:2000
ggplot(dat[size.sub,]) + geom_point(aes(x=num.mom,y=rec))
# it's harder to see the curve, unless you convert it
ggplot(dat[size.sub,])+ geom_point(aes(x=num.mom,y=rec/num.mom))
# better... but once more?
ggplot(dat[size.sub,])+ geom_point(aes(x=num.mom,y=log(rec/num.mom)))
# now it's linear, which is easiest to work with

# now what if we only have X years of these data...
n.years <- 20
x.years <- sample(size.sub,size=n.years)
ggplot(dat[x.years,]) + geom_point(aes(x=num.mom,y=rec))
# you can't discern a pattern from this... what if you transform it
ggplot(dat[x.years,]) + geom_point(aes(x=num.mom,y=rec/num.mom))
# that's better... once more?
ggplot(dat[x.years,])+ geom_point(aes(x=num.mom,y=log(rec/num.mom)))
# very nice and linear


