# Test statistician 2024

This repo contains the following files:
- GDP.csv - Country/year data for the indicator GDP (constant 2015 US$)
- population.csv - Country/year data for the indicator Population, total
- WB_groups.csv - Country groupings for the World Bank including by region

The indicators "GDP (constant 2015 US$)" and "Population, total" are both indicators in the World Bank's World Development Indicators (WDI) database. You can find more information on the WDI [here](https://datatopics.worldbank.org/world-development-indicators/). Specific information including metadata on the two indicators is available from the data site of the World Bank:  [NY.GDP.MKTP.KD](https://data.worldbank.org/indicator/NY.GDP.MKTP.KD) and [SP.POP.TOTL](https://data.worldbank.org/indicator/SP.POP.TOTL).

Don't commit any answers to the current repo, as all other candidates will be able to see your responses!

To submit the answers to this test you have two options: 
1)  create a private repo on your Github account and clone this repo (you can use this repository as a template). Commit changes to your cloned repo (your private repo) and provide access to your repo to thijsbenschop, mwelch750, stacybri (GitHub usernames). Then send the cloned repo link by replying to the email with the test instructions (to mwelch@worldbank.org copying bstacy@worldbank.org and tbenschop@worldbank.org). 
2)  download the files. Then send a zipped copy of all output files (excluding the original data files) by replying to the email with the test instructions (to mwelch@worldbank.org copying bstacy@worldbank.org and tbenschop@worldbank.org). 

You have 120 minutes (two hours) to complete this test. After 120 minutes you should have committed the last change to your cloned repo and sent an email with all your responses to mwelch@worldbank.org. If submitting answers by email, please attach all code/scripts you produced to prepare your responses. Responses have to be reproducible from your code/scripts.

Please reach out immediately to us by email if you experience technical issues. Please refrain from asking for clarifications about the questions.

This test consists of three (3) questions with subquestions. Make sure to provide responses to all your questions and submit the code you wrote to answer your questions. Both your responses and code will be evaluated. Comment your code to make it more readable.

You can add the answers to this md file in a cloned GitHub repository or use another format of choice. Make sure to provide explanations and interpretations if asked for them. We expect explanations to be no longer than a few paragraphs.

## Question 1
1) Read in the datafiles using a statistical software package (R or Python preferred).
2) Calculate a new indicator "GDP per capita" for all country/year pairs in the database for which this is possible.
3) Could you calculate the GDP per capita for each country/year pair in the database? Explain why (not).
	- I am able to calculate the GDP per capital for each country/year pair in the database when both GDP and population data was presented for each pair. I first transform both dataset into long format, then performed inner_join to merge the data, then did the calculation of "GDP/Population" for each row (Country + year combination) and saved the result in a column called "GDP per capita".
4) If you couldn't calculate GDP per capita for all combinations, explain how you could improve coverage in one paragraph.

## Question 2
Note: Errors may have been added to the population and GDP data in the data in this repo.

1) Calculate the average GDP per capita for the regions AFE (Africa Eastern and Southern) and AFW (Africa Western and Central) starting from the country level data. The definitions of the regions (which country belong to the two regions) are available in the file WB_groups.csv. This file contains all countries and regions as well as a column that specifies the region to which the country belongs.
2) Calculate summary statistics and visualize the differences and trends of GDP per capita within and between these two regions. Use an appropriate visualization to show trends over time as well as the differences between the two regions.
3) Explain which weight you used for the calculation of the regional averages and why.
	- When calculating the average GDP per capita, I used the total GDP within the region divided by total population, for each year.
4) Do the regional averages you have calculated represent all countries in these two regions Explain why (not). Explain how could you improve this in one paragraph? 
	- The regional averages I calculated may not represent all countries in these two regions, because of the incompleteness or the scarcity of the GDP data, which will cause incorrect weights and eventually inaccurate average GDPs. For example, Angola does not have any GDP data before 1979. In order to improve this, I would first try search for missing data from other reliable source (i.e. from country reports) and if that does not work, I would try to impute some of the data based on country historical data and trends (using time series)
5) What is the time span for which you calculated the regional averages? Why did you choose this time span? Explain in one paragraph.
	- For the regional average GDP, I selected data since 1984, because after 1984 the GDP data coverage exceeds 80 percent of the regional countries for both regions. Before 1984, data was too scarce and could not accurately represent the regional data.
6) Plot the AFW regional average together with all the countries in the AFW region in one plot. Compare the countries and comment on trends you observe. Please summarize the findings in one paragraph.
	- From the graph, we can see a huge difference in GDP per capita across AFW region, where countries like Gabon and Equatorial Guinea have way higher GDP per capita than the rest countries. When looking at regional average GDP per capita together with country data, it has a similar trend among majority of the countries within the region. 

## Question 3
1) Propose data quality tests for the GDP data and implement these in a script. Describe the tests you perform and explain why.
	- Run basic summary statistics when reading in the dataset, review the distribution, range, etc.
2) Did you find any issues with the data?
	- Yes, I found duplicated and incorrect data in WB_groups.csv. In the file, the 5th column was unnamed and has income-level groups. This may be caused by incorrect merging.
3) Describe a test for outliers in the data and implement it.
	- One way to visually inspect the data for selected country/region. For example, box plot graph, distribution graph.
	- Can also set a threshold based on standard deviation, and flag data away from, say 3* std.
4) Did you find any outliers? If so, describe how you would proceed\deal with these outliers before publishing the data.

## End of test
Don't forget to submit your results
