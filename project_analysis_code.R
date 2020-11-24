amphibians <- read.csv(file = 'C:/Users/Liam/Desktop/d. Fall 2020/STAT 512/Project/amphibians.csv',sep = ';', skip = 1)

#Assumption: there is no given number of frogs per area, only the presence of frogs. We assume more types of frogs present in an area 
  #corresponds to more frogs in an area overall.

#Comment: The larger the number of reservoirs, the more likely it is that some of them will be suitable for amphibian breeding.
#Comment: The vegetation in the reservoir favors amphibians, facilitates breeding, and allows the larvae to feed and give shelter. 
  #However, excess vegetation can lead to the overgrowth of the pond and water shortages. 

#Define short names
#Frogs
  gf  = amphibians$Green.frogs
  gcn = amphibians$Great.crested.newt
  bf  = amphibians$Brown.frogs
  ct  = amphibians$Common.toad
  fbt = amphibians$Fire.bellied.toad
  tf  = amphibians$Tree.frog
  cn  = amphibians$Common.newt
  amphibians$Total.Frogs = gf + gcn + bf + ct + fbt + tf + cn
  tot = amphibian$Total.Frogs
#Attributes
  veg = amphibians$VR

#Vegetation amount vs Suitability
  t0 = subset(amphibians,amphibians$VR==0)
  t1 = subset(amphibians,amphibians$VR==1)
  t2 = subset(amphibians,amphibians$VR==2)
  t3 = subset(amphibians,amphibians$VR==3)
  t4 = subset(amphibians,amphibians$VR==4)
  y <- c(sum(t0$Green.frogs), sum(t1$Green.frogs), sum(t2$Green.frogs), sum(t3$Green.frogs), sum(t4$Green.frogs))
  
plot(c(0,1,2,3,4), y)
points(c(0,1,2,3,4),c(sum(t0$Brown.frogs), sum(t1$Brown.frogs), sum(t2$Brown.frogs), sum(t3$Brown.frogs), sum(t4$Brown.frogs)))
