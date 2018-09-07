#randomly generates 10000 X-coordinates
x=runif(10000)

#randomly generates 10000 Y-coordinates
y=runif(10000)

#generates the distance of point (x,y) from centre(0.5,0.5)
distance=sqrt((x-0.5)^2+(y-0.5)^2)

#finds the proportion of points inside the circle to the total number of points or points inside square
ratio = length(which(distance<=0.5))/length(distance)

#the ratio is multiplied by 4 to get the value of pi as per the equation stated above
val=ratio*4
val
