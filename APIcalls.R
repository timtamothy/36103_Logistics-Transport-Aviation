# Load packages ---- 

library(plumber)
?plumber


#### Endpoints for Amadeus include GET and POST
#### Pipeline = Filter, Parser, Endpoint, Serializer


#### Request an access token
#### Send post request to:
#### https://test.api.amadeus.com/v1/security/oauth2/token
#### With header:
#### Content-Type: application/x-www-form-urlencoded
#### With body:
#### grant_type=clinet_credentials&client_id=API_Key&client_secret=API_Secret

