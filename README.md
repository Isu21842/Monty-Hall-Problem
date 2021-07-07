# Monty-Hall-Problem
Agent based model of the Monty Hall game show problem and similar to Monte Carlo simulations.

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

