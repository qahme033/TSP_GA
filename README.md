# Instructions for running the code

Below are the instructions for how we ran the code for the Ant Colony Optimization and Genetic Algorithm.

# Ant Colony Optimization

The code for ACO can be found in ACO_algorithm.r

To run the code, there are 3 points at which you can change parameters.

Line 31 - number of iterations. Change this to match the number of iterations you want to run the algorithm for
Line 32 - number ants. Change this to set the number of ants in the simulation
Lines 100-105 - TSP instance. THere are 3 possible instances, Western Sahara (wi29), Djibouti (di38) and Zimbabwe (zi929), comment out the 2 you are not interested in using and uncomment the one you are.

To run the code, use the command `R < ACO_algorithm.r --no-save`

The results will be printed to the terminal and the best tour will be saved to Rplots.pdf

# Genetic Algorithm

The code for GA can be found in TSP_GA_algorithm.py

To run the code, use the command `python TSP_GA_algorithm.py`. The results will be printed to the terminal and a PDF will be generated at Rplots.pdf

# ACO + GA

The code for ACO + GA can be found in TSP_ACO+GA.py

To run the code, you must first follow the steps to run ACO.

Once you have generated a tour for ACO, it will be saved in myData.txt

Next, use the command `python TSP_ACO+GA.py` to run GA on the resulting ACO tour. The results will be printed to the terminal and a PDF will be generated at Rplots.pdf

# References

The ACO algorithm is our R implementation of an already existing algorithm written in MATLAB that can be found at http://www.mathworks.com/matlabcentral/fileexchange/15049-solving-tsp-with-ant-colony-system

The GA code is copied from https://gist.github.com/turbofart/3428880 which is based on Java code by Lee Jacobson found in an article entitled "Applying a genetic algorithm to the travelling salesman problem" that can be found at: http://goo.gl/cJEY1