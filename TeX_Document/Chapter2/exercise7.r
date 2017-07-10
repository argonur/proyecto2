# A really animal way of ploting the parabola. 
# You should normally not make your computer do such things!

# Range as function of height x(y)

#Parameters and constants
# Burnout altitud, range and velocity
ybo<-300 #km
xbo<-100 #km
vbo<-5 #km/s
#Earth's gravitational acceleration
g<-0.00981 #km/s²
#Angle
theta_grad<-75 #°
theta_rad<-theta_grad*(pi/180) #radians

# y vector
y<-seq(0,1500,0.01)
y_plus_ybo<-y+ybo
y2<-seq((0-ybo),1500,0.01)
y2_plus_ybo<-y2+ybo

#allocate memory for all the y(x)
x1<-matrix(nrow=1,ncol=length(y))
x2<-matrix(nrow=1,ncol=length(y2))
                      
var1<-(sin(theta_rad)^2)-((2*g*y)/vbo^2)
x1<-((vbo^2)/g)*(cos(theta_rad))*(sin(theta_rad)-sqrt(var1))+xbo

var2<-(sin(theta_rad)^2)-((2*g*y2)/vbo^2)
x2<-((vbo^2)/g)*(cos(theta_rad))*(sin(theta_rad)+sqrt(var2))+xbo

dataframe_75_1<-data.frame(y_plus_ybo,x1)
dataframe_75_2<-data.frame(y2_plus_ybo,x2)

#Plot
plot(dataframe_75_1, 
     col="red", 
     type="l", 
     xlim=c(0, 1500), 
     ylim=c(0, 1500),
     panel.first = grid(), 
     main="Range as function of height for launch angle 75°",
     xlab="y+ybo [km]",
     ylab="x [km]")

lines(dataframe_75_2, col="red")

legend(1000, 1500, 
       c("xbo = 100 km","ybo = 300 km","vbo = 5 km","theta = 75°"), cex=0.8);

text(450, 70, "MECO (ybo,xbo) ", cex=0.8);

#a doted line for MECO altitude and a point at MECO
a<-seq(from=0, to=3000, by=50)
b<-rep(ybo, times=length(a))
lines(b, a, pch=22, lty=2)
points(ybo,xbo)
