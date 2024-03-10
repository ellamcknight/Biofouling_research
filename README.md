# Biofouling research

### *Research from chapter 4 of my PhD* 😃

This research investigates contemporary climate change and non-native species (NNS) in marine brackish ecosystems. The focus is on extreme events such as warming and freshening (a sudden decline in salinity). 
Along coastlines, increased rainfall events may alter the dynamics of tidal and riverine ebb and flow oscillations. 
The repercussions of intense and prolonged declines in salinity may alter resource availability, species' competitive ability and overall ecological dominance within marine/brackish communities. 

We deployed settlement panels in two locations in Plymouth Water (Millbay & Queen Anns Battery Marinas) distinguished by different salinity regimes, one with stable salinity regimes and the other with greater salinity fluctuations. To detect salinity dynamics we deployed high-resolution salinity metres in the study sites. Other environmental factors were recorded and measured.

## Analytical Techniques 
<pre>
◾ R 
◾ Generalised Additive Mixed Models
◾ Non-Metric Multidimensional scaling
</pre>

## Environmental data

Initial data cleaning and visualisations showed anomalies in the salinity and temperature data. Salinity was affected by drift. The temperature was affected by erroneous data points due to the hardware being removed for anti-fouling or data recovery.

Temperature was corrected by adding data from the light metre which recorded temperature. Metres where from [HOBO](https://www.onsetcomp.com/products/data-loggers/u24-002-c) & [HOBO](https://www.onsetcomp.com/products/data-loggers/ua-002-64)

The figure below shows a daily summary of a) minimum salinity, Millbay: grey & Queen Anne’s Battery: blue, b) mean temperature, Millbay: grey & Queen Anne’s Battery: red, and c) peaks show daily total precipitation.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/environs.png?raw=true)

To correct the salinity data I modelled salinity over time, extracted the residuals and incorporated these data into the model. There were also periods of erroneous data due to metre malfunctioning causing the data to flat line at zero; these data were removed. Below are the salinity residuals from running a linear regression on the salinity measurements for (a) Millbay and (b) Queen Anne’s Battery. These data were implemented into the Generalised Additive Mixed Models.

![alt text](https://github.com/ellamcknight/Biofouling_research/blob/main/Images/residuals.png?raw=true)

## Data Preparation







