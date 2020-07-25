# Microsimulator
This is the combined code for the microsim. It consists of four major components:

### Simulator
The engine that performs the simulation

### ABF
The module that provides analysis on the costs and tax revenue

### GUI
A interface for users to set program, population behavior, and simulation parameters

### Driver
The main module that ties the other components together

## To Do
* Consolidate FMLA and ACS cleaning code into one module. Currently, the Simulator and ABF components are doing their own cleaning. It would be better to have one separate component that deals with cleaning.
* Create a GUI using Qt, which looks better visually but might be harder to run.