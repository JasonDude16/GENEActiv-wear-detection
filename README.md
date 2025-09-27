**Problem**: Our current way of analyzing GENEActiv Actigraphy data (GGIR) is not very good at detecting off-wrist (OW) data
- It detects OW data overnight when participants are sleeping. This time can be imputed by changing a line in the GGIR Code. 
- It also will detect OW data around BT and WT but which can significantly mess with our ppt bed and wake times. 
- The program only uses actigraphy data to determine if the watch is off-wrist despite the watch also giving us temperature data. 
- The program is also really bad at showing us when it considers the watch OW, the best way we have found to look at it is through an “unreliable” visual data output

**Temporary solution**
- We are able to get the raw temperature data output and can see generally when the watch is OW or not. This is a manual process and requires human input which can cause discrepancies in how the data is looked at. 
- Also, we are unable to alter the output that GGIR gives us so this basically tells us if we can include a night of data or not, it does not give us the option to alter data based on if the watch is on or off. 

**Goal solution**: make a better algorithm for identifying OW time for the GENEActiv watch and then be able to add in this algorithm into the GGIR algorithm. 
- Ideally this is a machine learning algorithm based on true OW data. We are loosely basing this on this article https://pubmed.ncbi.nlm.nih.gov/39788836/ which uses a different actiwatch and different ground truth. 

**Next steps**:
1. Merge the raw actigraphy output from GGIR (this has the temp data) and combine with the raw output from the GENEAciv software (this has the activity counts)
2.	Extract the dynamical features (using the tsfeatures package in R) using time, activity counts, and temperature. 
3.	Use the 3.0-6 version of GGIR
