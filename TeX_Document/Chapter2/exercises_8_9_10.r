# Exercise 8

# A rocket is launched with a burnout velocity of 75 m/sec, burnout altitude of 300 m, 
# and a burnout range of 100 m. Assuming a flight path angle of 75°, 
# calculate the final range of the rocket when it impacts the ground.

# Given data
vbo<-75 # m/s
xbo<-100 # m
ybo<-300 # m
theta_grad<-75 # °
g=9.81 # m/s²

##############
theta_rad<-theta_grad*(pi/180) # radians

######## maximum range calculation #########
var1<-vbo*cos(theta_rad)
var2<-((vbo*sin(theta_rad))+sqrt((vbo^2*sin(theta_rad)^2)+(2*g*ybo)))/g
x_max_75<-xbo+var1*var2

# Exercise 9

#Calculate the maximum altitude reached by the rocket in Exercise 8.
var3<-vbo^2*sin(theta_rad)^2
y_max_75<-ybo+var3/(2*g)

# Exercise 10

# Redo Exercise 2.8 to determine the range at MECO altitude. What is
# the range at MECO if the initial flight path angle is 15°?

theta_grad<-15 # °
theta_rad<-theta_grad*(pi/180) # radians

var4<-vbo*cos(theta_rad)
var5<-((vbo*sin(theta_rad))+sqrt((vbo^2*sin(theta_rad)^2)+(2*g*ybo)))/g
x_max_15<-xbo+var4*var5

y_meco<-ybo

var6<-(sin(theta_rad)^2)-((2*g*(y_meco-ybo))/vbo^2)
x_meco_15=((vbo^2)/g)*cos(theta_rad)*(sin(theta_rad)+sqrt(var6))+xbo

#now x_meco for 75°

theta_grad<-75 # °
theta_rad<-theta_grad*(pi/180) # radians

var7<-(sin(theta_rad)^2)-((2*g*(y_meco-ybo))/vbo^2)
x_meco_75=((vbo^2)/g)*cos(theta_rad)*(sin(theta_rad)+sqrt(var7))+xbo


# Plot of the two trajectories

#Angles
theta_grad<-c(75,15) #°
theta_rad<-theta_grad*(pi/180) #radians

# x vector
x<-seq(0,3000,5)
x_plus_xbo<-x+xbo

#allocate memory for all the y(x)
y<-matrix(nrow=length(theta_grad),ncol=length(x))

# Loop to generate y(x) vectors for different theta angles
i<-1
while (i <= length(theta_rad)) {
  y[i,]<-ybo+tan(theta_rad[i])*x-(g/(2*vbo^2*(cos(theta_rad[i]))^2))*x^2
  i<-i+1
}

#TODO generate the dataframes in the loop
dataframe_75<-data.frame(x_plus_xbo,y[1,])
dataframe_15<-data.frame(x_plus_xbo,y[2,])

#Plot
# TODO labels to each line to be able to identify them, get xlim and ylim from the maximum values instead of hard coded

plot(dataframe_75, 
     col="red", 
     type="l", 
     xlim=c(0, 900), 
     ylim=c(0, 600),
     panel.first = grid(), 
     main="Trajectory of a rocket lauched at 75° and 15°",
     xlab="x+xbo [m]",
     ylab="y [m]")

lines(dataframe_15, col="blue")

legend(600, 600, 
       c("75°","15°"), cex=0.8,
       col=c("red","blue"),
       lty=1);

text(150, 250, "MECO (xbo, ybo) ", cex=0.8);


#a doted line for MECO altitude and a point at MECO
a<-seq(from=0, to=3000, by=50)
b<-rep(ybo, times=length(a))
lines(a, b, pch=22, lty=2)
points(100,300)
