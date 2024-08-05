# Portfolio-Valuation-
## Quantitative Balance Sheet Strategy

## Documentation

[Project Link- https://hammadrkh.wixsite.com/portfolio/projects/quant-finance-modelling-in-r)

# Implementing Quantitative Balance Sheet Strategy of e-merchant data to:

-Create a valuation document for a fintech loan portfolio.
-Analyze historical data and compute repayment rates.
-Forecast cash flows in R to find the portfolio's present value.

My Assumptions of how I arrived at this 30-month repayment period is simpflified in the powerpoint/presentation image located at the end of this project in the gallery.



Today, credit is no longer limited to classical bank loans. The rise of e-commerce has increased the demand for new and more flexible credit solutions. The increased demand has been met by new companies from the fintech sector that offer easily accessible online loans to a wide audience. These new business models offer simple solutions, such as flexible payment schedules or buy-now-pay-later, that can be accessed with just a few clicks. 


Online merchants also need new ways to procure capital to manufacture their products, as they receive payment only after their product has been sold. In order to meet increasing demand, new credit offerings have emerged. Repayment of these loans is no longer based on a fixed schedule but instead depends directly on online sales. The loan is paid back in instalments with every sale, and thus payments depend directly on the sales volume. 


Predicting the future cash flows needed for the valuation of a portfolio consisting of these merchant loans is challenging. 


This data set came from a large global online lending platform that provides loans to both consumers and merchants. These instruments are classified as assets on the balance sheet.

 

We are going to ensure the porftfolio has been valued correctly and perform due dilligence that the balance sheet values are correct. The value of the loan portfolio depends on future cash flows, which are stochastic. 


The following steps will serve as a guide for my methodology of how to Quant the final Valuation: (I would reccomend either using R, Python, Excel for this but it can be done in other tools as well with comfort of syntax being the factor for consideration). 


Inspect the historical data provided by the client. The data ranges from June 2019 until December 2020. Every month constitutes a vintage and the data includes the loan amount that was originated per vintage, as well as the repayments that have been observed up until and including December 2020 (the vintages are given as rows and the columns specify the period of the repayment).

Based on the provided data, compute the historical repayment percentages, i.e. every repayment’s share of the origination amount.

Compute the expected repayment percentages for all vintages over the lifetime of the loans. Details on how the expected repayment percentages are to be computed can be found in the attached assumptions PDF below.

From the expected repayment percentages, compute the forecasted cash flows using the origination amounts.

Using the assumed discount rate, derive the present value of the forecasted cash flows and of the portfolio. Don’t forget to convert the annual interest rate to a monthly interest rate.

## Data Set is Located in Files Repository

**** I am going to personaly model this forecast in R due to it's need for statistical accuracy, expected repayment percentages taking into account the series of discount factors with thier possible effects on future cash flows, expected repayment percentages of correct payment period, vector of amounts originated per vintage, etc.

```R
`data` <- read.csv("Data.csv",sep=";") 

n.forecast <- 30 #number of forecast periods
n.vintage <- nrow(data) #number of vintages
discount.rate <- 0.025  #discount rates (annual)


historical.cf <- as.matrix(data[,3:ncol(data)]) #matrix of cash flows that have already been observed
amount.originated <- data[,2] #vector of amounts originated per vintage


periods.remaining <- n.forecast - n.vintage:1 #number of periods to be forecasted per vintage

paid.percentages <- historical.cf/amount.originated #repayment percentage, i.e. historical payments as a percentage of the originated amount per vintage
first.period <- diag(paid.percentages) #repayment percentage in the period that the loans were originated per vintage
second.period <- c(diag(paid.percentages[-nrow(paid.percentages),-1]),as.numeric(paid.percentages[n.vintage,n.vintage]*2)) #cash flow percentage in the period after the loans were originated per vintage
#(assume the second cash flow is twice the first cash flow for the last vintage)


p <- matrix(0,nrow = n.vintage, ncol = n.forecast) #marix of zeros
p[,1] <- first.period #assign repayment percentage of first period
p[,2] <- second.period #assign repayment percentage of second period


for (i in 1:n.vintage){ #calculate the expected repayment percentages according to the formula (columns are to be interpreted as periods since origination and not months)
        for (j in 3:n.forecast) {
                p[i,j] <- max(0, p[i,2] * log( 1 + (1-sum(p[i,1:(j-1)]))) * (1-(j-1)/n.forecast) )
        }
}


p.forecast <- matrix(0,n.vintage,n.forecast-1) #matrix of zeros for the forecasted xpected repayment percentagess (each column corresponds to a period in the future, starting in January 2021)
for (i in 1:n.vintage){ #assign the expected repayment percentages to the correct periods
        for (j in 1:periods.remaining[i]) {
                p.forecast[i,j] <-  p[i,n.forecast-periods.remaining[i]+j]    
        }  
}


discount.factors <- 1/(1+discount.rate)^((1:(n.forecast-1))/12) #series of discount factors 
pv <- t(t(p.forecast)*discount.factors)*amount.originated #present value of forecasted cash flows   
result = sum(pv) #sum of all present values, i.e. the value of the portfolio


paste0("The fair value estimate for the portfolio is ", round(result,2)," Swiss Francs")
 
'The fair value estimate for the 
portfolio is 84779941.82 Swiss Francs'
```
The client’s estimate of the portfolio value was CHF 84’993’122.67. After Computing both the absolute and relative difference it is important to consider that an audit team will likely consider any difference smaller than CHF 500’000 to be acceptable given the size of the portfolio. 


When attempting this yourself, the most important question and take home message a Quant must consider is that based on the result of the valuation, can one conclude whether the difference to the client’s estimate falls below his threshold, as appetitie for risk is not one size fits all? 
