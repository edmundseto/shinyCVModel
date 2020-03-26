An R Shiny app for Washington Novel Coronavirus Modeling

Edmund Seto
eseto@uw.edu
3/20/2020

It's running online here:
https://shiny.deohs.washington.edu/app/cvmodel

You can also download the code, and run the R Shiny app locally on your computer in R Studio.

In the app.R file is code for SIR and SEIR models, which are dynamic epidemiologic models
that model the flow of people from susceptible (S), exposed (E), infectious (I), and 
recovered (R) states. For my SEIR model I assumed non-COVID-19 mortality is zero
for simplicity. There are various examples of running the models to evaluate different
interventions scenarios. 

Some of the questions I explore with the model are:

1. What does the situation look like in WA state?
     Washington State Case Data
     
2. Can we fit a model to the WA state data?
     Model Parameter Fitting to Data
     
3. If we do nothing, how many will be infected?
     Infections Without Any Intervention
     
4. But not all infections result in symptoms -- How many people will be sick? How many will die?
     Illnesses and Deaths in the Absence of Intervention

5. What is this 'flattening the curve' I keep hearing about in the news?
     Flatten the Curve, Don't Overwhelm Healthcare Resources
     
6. Does isolating sick people help reduce transmission?
     Isolating Sick People
     
7. Aside from flattening the curve, don't we need to minimize total cases and deaths
     Minimizing the Cumulative Numbers of Sick People and Deaths
     
8. Does 'social distancing' reduce transmission?
     Shut-downs, Working Remotely, Shelter-in-Place, and the Like

9. Does the timing of interventions matter?
     Swift, decisive action to implement interventions

10. I hear people talk about R0 for the virus -- what's that about?
     The Basic Reproduction Number, R0
     
11. Why can't I get tested for COVID-19? Shouldn't everyone be tested?
     The Importance of Early, Large-scale COVID-19 Testing
     
12. Why is a seroprevalence test even more important than a COVID-19 diagnostic test right now?
     Information on How Many People in the Community Have Been Exposed is Critical to Planning Interventions

13. When can we stop interventions, and return to our normal lives?
      Be Careful About Terminating Interventions Too Early
      
14. Should we shut down schools? What about shutting down (fill in the blank)?
      Limiting High Contact Sites
      
15. Should we limit travel?
     Imported Disease      
     
16. If we institute a 'shelter-in-place' policy in one city, but not other cities, will it still be effective?
     Disease Persistence Across Metapopulations
     
17. What about an SEIR model that considers exposure latency?
     Fitting an SEIR Model
     
Feel free to email me a note if you find this useful.

