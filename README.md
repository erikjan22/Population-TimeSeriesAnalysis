## Project
The script is used to analyse time series data of North American rodent populations using various models. The data was collected for twenty years between 1979-1999. Since the most amount of observations was made for the North American deer
mouse (*Peromyscus maniculatus*) (13304 observations) and the Eastern chipmunk (*Tamias striatus*) (8401 observations), the script selects those as default species. It was found that the best fitted models for these two species were respectively a Moving Average model with three parameters and a SARIMA(1, 0, 5) × (0, 1, 1)12 model. This makes sense, because while the deer mouse is active throughout the year, the eastern chimpunk is a more seasonal creature and spends a large amount of time in its burrow during the winter months.

The data spans is available [online](https://ecologicaldata.org/wiki/powdermill-biological-station-small-mammal-database).

## Results
By choosing a model in the script, you can create a prediction for the selected rodent species. Two examples are visible here:

![PM_pred.png](https://github.com/erikjan22/Population-TimeSeriesAnalysis/blob/master/PM_pred.png)
![TS_pred.png](https://github.com/erikjan22/Population-TimeSeriesAnalysis/blob/master/TS_pred.png)

