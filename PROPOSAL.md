# Proposal: Unemployment rates 

## Section 1: Motivation and Purpose
  
Unemployment rates, defined as the number of cilivian labor force divded by the number of unemployed but are actively seeking work (Amadeo, 2019), can inform us on multiple factors for a country or an industry. It can inform us on the rise and fall of a country’s economic condition and the rate can act as a lagging indicator to help explain certain economic slowdowns (Blank, 2008). It can also help inform us on the trend and job market of an industry and whether people are having difficulty finding a job in a specific field. The purpose of our app is to show which industries have been decreasing or increasing in the number of jobs available. We would expect the industries that are implementing more automation to have higher rates of unemployment. Understanding unemployment trends could help us address economic challenges and determine which industries are facing job losses or gains. We hope that our dashboard could be used as a framework for other analysts to interpret and understand unemployment rates in respective industries and countries. 
	
## Section 2: Description of the data

We will be visualizing the Unemployment Across Industries dataset gathered from the [vega datasets](https://github.com/vega/vega-datasets) with 6 variables and 1708 observations. The `series` column gives us the 14 industry types in the dataset. The industries in the dataset are:

* Government, Mining and Extraction, 
* Construction, Manufacturing, 
* Wholesale and Retail Trade, 
* Transportation and Utilities, 
* Information, 
* Finance, 
* Business services, 
* Education and Health, 
* Leisure and hospitality, 
* Other, 
* Agriculture, 
* Self-employed

 There is also a `year` column with the years 2000 and 2010 and a `month` column. The monthly unemployment information across various industries from 2000 to 2010 of an unknown area. It is unclear whether this area is a town, city, or country and for the purpose our app, we will call it "Country X". This should not minimize the impact of our app as it acts as a framework for other countries and industries to use for their interested unemployment rates. Further, the dataset contains the unemployment rates in two forms: the number of people who are unemployed (`count`) and the rate of unemployment (`rate`). We presume that the rate of unemployment is the proportion of people who are unemployed in a specific industry for that given year. 

## Section 3: Research questions and usage scenarios

Our dashboard works to answer questions on industries unemployment trends with visualizations. Our dashboard could serve as a framework for officials to gain insight into their countries industry employment trends and implement changes pertaining to the results. For instance, the officials can change policies such as cutting taxes for certain industries that have higher unemployment rates. Our research questions are as follows: 


1) Which industry has grown/shrunk the most?

2) How does overall unemployment change in country X over the years?

Comparing the overall unemployment change for the country as a whole to the the rise and fall of unemployment rate for a specific industry could enlighten us on the desire to work for this industry. That is, if the overall unemployment rate has not changed for the country as a whole in the given year but increased for a certain industry, this could mean that people are moving away from this job field and finding work in other industries. This could help countries determine which job markets government fundings should focus on and which industries can help with their economic growth. 

3) Is the unemployment rate across industries seasonal?

This would be interesting to see if unemployment rates are affected by the season and see when the job markets are high and low across industries. It can also help explain when businesses are slow or competition is high.

<b> References </b>

* Amadeo, Kimberly (2019). Unemployment rate effect and trends., the balance. https://www.thebalance.com/unemployment-rate-3305744 
 
* Blank, Rebecca., 2008., What the unemployment rate signals on the economy., Brookings. https://www.brookings.edu/opinions/what-the-unemployment-rate-signals-on-the-economy/


