# I am goign to use an example of a Cox proportional hazards model to obtain transition probabilities for a MDP with 5 time waves:

import pandas as pd
from lifelines import CoxPHFitter

# Step 1: Define the states of your MDP in terms of the variables that are associated with the hazard of the event of interest.
states = ["Healthy", "Ill"]

# Step 2: Collect data on the event of interest and the covariates of interest.
# Assuming you have dataframe 'df' with columns 'T' and 'E' as time-to-event and event indicator and 'covariate1', 'covariate2'

# Step 3: Use a Cox proportional hazards model to estimate the hazard function for the event of interest as a function of the covariates.
cph = CoxPHFitter()
cph.fit(df, duration_col='T', event_col='E', show_progress=True)

# Step 4: Convert the hazard function estimates into conditional transition probabilities
# Using the survival function of the model
# The survival function gives the probability that a subject will survive past time t, given they are still at risk at time t.

# For example, let's assume the time is t = 10 and the subject is at state 'Healthy'

# time_waves = [5, 10, 15, 20, 25]
# Creating a dictionary of survival probabilities for each time wave
transition_probabilities = {}
for t in time_waves:
    healthy_to_ill_prob = 1 - cph.predict_survival_function(df[df['covariate'])
    transition_probabilities[t] = healthy_to_ill_prob
    
# Step 5: Use these transition probabilities to update the state-value or action-value function of the MDP.

# For example, you can use these probabilities to calculate the expected value of the MDP at each state and each time wave
for t in time_waves:
    for state in states:
        expected_value = 0
        for next_state in states:
            expected_value += transition_probabilities[t][state][next_state] * value_function[next_state]
        value_function[state][t] = expected_value



time_waves = [5, 10, 15, 20, 25]

# Creating a dictionary of survival probabilities for each time wave
transition_probabilities = {}
for t in time_waves:
    state_to_state_probabilities = {}
    for state in states:
        state_to_state_probabilities[state] = {}
        for next_state in states:
            state_to_state_probabilities[state][next_state] = 1 - cph.predict_survival_function(df[df['covariate'] == state]).loc[t].values[0]
    transition_probabilities[t] = state_to_state_probabilities

                                                               
# Lol I am going to try this when I am not as tired and it's not 2am in the morning... I think I made a mistake with this code but we shall see
                                                               
                                                               
