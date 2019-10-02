# ######Monty Hall Problem Model######
# 
# ##Purpose##
# Compare strategies within the Monty Hall Problem. Agents will take turns playing the guessing game from the Monty Hall problem.
# The model will determine and store how the agent would have fared in each iteration had they used a stay or a switch strategy.
# 
# ##Agents##
# Each iteration of the model will generate a single agent who will play one round of the guessing game from Lets Make a Deal.
# The agent will use both a stay and switch strategy in each round
# 
# ##Life Cycle##
# 1. Setup
# 2. Guess
# 3. Reveal Goat
# 4. Stay/Switch
# 5. Reveal Car
# 
# #Setup#
# At the start of the life cycle, "Monty Hall" will place a car behind one randomly selected door and goats behind the remaining doors
# 
# #Guess#
# During this stage, the agent will guess which door conceals the car. The agent will pick a random door each round.
# 
# #Reveal Goat#
# After the agent's initial guess, Monty Hall will reveal what is behind one of the doors. The revealed door will always (1) conceal a goat and (2) not be a door the agent has guessed.
# 
# #Stay/Switch#
# At this stage, Monty Hall will ask the agent whether it wants to stay with its original guess or switch to the other, unrevealed door.
# The model will separately store the result of both decisions
# 
# #Reveal Car#
# At this stage, Monty Hall will reveal which door concealed the car. If the agent's decision was correct, the model will store this as a victory for that round. If the agent's decision was incorrect, the model will store this as a loss for that round.
# 
# ##Analysis##
# The model will compare the proportion of games in which the agent won the car with both strategies



#####Packages#####
library(ggplot2)





######Model Parameters######

modelloops<-10000




######Model Start######

#Create a blank data
result<-data.frame(matrix(NA,modelloops, 2))
colnames(result)<-c("stay","switch")

for(m in 1:modelloops){
  ###Set up###
  
  #Create a vector of door numbers...don't forget to run each variable!
  number<-1:3
  #Create a vector for the prizes
  prize<-c("car","goat","goat")
  #Tell R to randomly scramble the prize order
  prize<-sample(prize)
  #Create the doors by putting these two vectors together
  doors<-data.frame(number,prize)
  
  #Guess#
  #Have the agent guess a random door number
  startguess<-sample(doors$number,1)
  
  
  
  ###Reveal Goat###
  #What I just told R: Create a new vector called reveal. Reveal a door number in 
  # which the prize associated with it is "goat".
  reveal<-doors$number[doors$prize=="goat"]
  #If applicable, remove the door that the agent guessed. This is applicable
  # when the agent picks a car making their two doors left with "goat"
  reveal<-reveal[reveal!=startguess]
  #Finally, if more than one valid door remains...
  if(length(reveal)>1){
    #...sample one of the valid doors at random
    reveal<-sample(reveal,1)
  }
  
  #Record the agent's guess if they decide to stay
  stayguess<-startguess
  #Determine the agent's guess if the decide to switch
  #This is the door that was not originally guessed or revealed
  switchguess<-doors$number[!(doors$number%in%c(reveal,startguess))]
  
  ##Reveal Car##
  #Determine the prize if the agent decides to stay
  #Go to doors$prize but give me the door number for which they gussed
  stayresult<-doors$prize[doors$number==stayguess]
  #Determine the prize if the agent decides to stay
  switchresult<-doors$prize[doors$number==switchguess]
  #Now R will observe these two statements that say if stayresult or switchresult
  #is a car then it is true
  result[m,1]<-stayresult=="car"
  result[m,2]<-switchresult=="car"
}



######Plotting Results######
#Genereate a dataframe for plotting the barplot
plotdata<-data.frame("strategy"=c("stay","switch"), "result"=colMeans(result))
#Plot the barplot
plot1<-qplot(strategy,result,data=plotdata,xlab="Agent Strategy",ylab="Results",fill=strategy,geom="blank")+geom_bar(stat="identity")
#Plot the cumulative performance of the switch strategy
plot2<-qplot(1:modelloops,cumsum(result$switch)/1:modelloops,xlab="Model Run",
      ylab="Cumulative Performance (Switch)",geom="line")+theme_classic()

