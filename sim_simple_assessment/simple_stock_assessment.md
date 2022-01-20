You are a stock assessment scientist given a time series of catches and catch-per-unit-effort (CPUE) from a local babelfish fishery. Your goal is to try and estimate the status of this population by fitting a model to the provided CPUE data. 

You'll do this by adjusting the intrinsic growth rate (*r*) and carrying capacity (*K*) to try and get the red lines in the CPUE plot to match up with the blue points. 


The model you will use is

$$b_y = b_{y-1} + b_{y-1}r(1 - \frac{b_{y-1}}{K}) - catch_{y-1}$$

$$\hat{cpue_t} = q \times b_y$$

Where *b* is the estimated biomass, *r* is the intrinsic growth rate, and *K* is the carrying capacity. *catch* are the observed catch data. $\hat{cpue_t}$ is the CPUE estimated by your model in a given year *y*

