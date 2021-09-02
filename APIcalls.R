# Load packages ---- 

library(httr)
library(jsonlite)

#### Endpoints for Amadeus include GET and POST
#### Pipeline = Filter, Parser, Endpoint, Serializer


#### Request an access token
#### Send post request to:
#### https://test.api.amadeus.com/v1/security/oauth2/token
#### With header:
#### Content-Type: application/x-www-form-urlencoded
#### With body:
#### grant_type=clinet_credentials&client_id=API_Key&client_secret=API_Secret

#### Get the Airport Data from ICAO API ----
icao <- GET(
  url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=8d00ef90-0982-11ec-9d72-8160549d64ab&airports=&states=USA'
)

#### Get response JSON content ----
response <- content(icao, 'parsed')

#### Convert to data tibble from JSON ----
data <- fromJSON(response)



summary(data)