# Hask-Ale :beer:

## Pre-requisites

### Stack

Install `Haskell Tool Stack`, see [Haskell Tool Stack Installation](https://docs.haskellstack.org/en/stable/README/)

### LZMA Library

You may need to install manually the `lzma` library used by the `Swagger` library:

- Fedora: `sudo dnf install ghc-lzma-devel`
- macOs: `brew install xz`

### [macOs] Libz

For macOs users, you have to re-install `libz` properly following these steps: 

```
rm -f /usr/local/lib/libz*
brew install lzlib
```

## Start 

### Run the tests

```
stack test
```

You can add the `--file-watch` option to reload automatically the tests after each modification.

### Run the application

```
stack run
```

You should see the following message:

```
listening on port 3000
```

The Swagger is available at `http://localhost:3000/swagger-ui`

## Steps 

### Step 0 - Ping... pong

[Lib.hs](src/Lib.hs)
Create a first endpoint to ping our server: **GET /ping** => Returns `"pong"`

- First you need to define your `PingApi` type as below:

```haskell
type PingApi = "ping" :> Get '[???] ??? -- GET /ping
```

- Secondly, you need to create the handler which will return the expected feedback when the ping endpoint is called: 
```haskell
pingHandler :: AppContext ??
pingHandler = return ??
```

- Then you need to define a function which creates your server and which holds the function that will handle the call `pingHandler`:

```haskell
server :: Server Ping
server = ??
```

- Finally you should define a function which creates your application handling the `PingApi` proxy and server defined before:
```haskell
mkApp :: Application
mkApp = serve (Proxy :: Proxy PingApi) server
```

This function is called in the `runServer` entry-point as an argument to run the server with provided default settings.

**Expected result:** 
```sh
$ curl http://localhost:3000/ping
Pong
```

Good job! You defined your first http endpoint in Haskell with Servant! 

### Step 1 - Model to JSON (Aeson - encode)

In [Model directory](src/Model), we have a simple model definition with a `Beer` and `BeerStyle` modules. 

We know we want to deal with this model as JSON as output so we have to define a way to encode these models to JSON. 

To do so, we are going to use [Aeson](http://hackage.haskell.org/package/aeson) which has already been added as dependency to the project. 

Some tests are already defined to validate the implementation so your goal is to propose a way to get the model as encoded.

- First, you need to uncomment the following tests in [test/Model/BeerSpec.hs](test/Model/BeerSpec.hs): 
```haskell
test_encodeValidBeerWithoutAlcoholRate =
  testCase "When encoding a beer without alcohol rate, it returns a JSON" $
  encodePretty' encodePrettyOptions beerWithoutAlcoholRate @?= beerWithoutAlcoholRateAsJson

test_encodeValidBeerWithAlcoholRate =
  testCase "When encoding a beer with alcohol rate, it returns a JSON" $
  encodePretty' encodePrettyOptions beerWithAlcoholRate @?= beerWithAlcoholRateAsJson
```
- Run the tests: `stack test --file-watch`
- Then you can update [src/Model/Beer.hs](src/Model/Beer.hs) to fix the tests

A tip: `encode :: ToJSON a => a -> ByteString`

**Expected result:** the tests should pass.

### Step 2 - Get all beers!

So now, how to get all the beers? 

In [src/Constants.hs](src/Constants.hs) there is a `beers` function which returns a list of beers we want to return when calling `GET /beers`.

- Create a `ApplicationAPI` type as below: 
```haskell
type ApplicationApi = PingApi :<|> "beers" :> Get '[ JSON] [Beer]
```
In this type, we get the previously created `Ping` and we define a new endpoint over `/beers` using the `Get` request. This return a JSON formatted list of `Beer`.

- Create the associated handler of the newly created endpoints:
```haskell
beersHandler :: AppContext [Beer]
beersHandler = ??
``` 
- Change server signature to: `server :: Server ApplicationApi` as now, it does not only handle the `ping` but the `ApplicationAPI`. 

- Update the code to get the compiler happy (you can combine handlers with `:<|>`). What happens if you change the order? Why?

- Update the function `mkApp` to return the right type

- Run the application and open your browser to check `http://localhost:3000/beers`

**Expected result:** it should return the list of beers as a JSON array 

### Step 3 - Find the beer

Let's get a specific beer by its `id` now: `GET /beers/{searched_id}`

- Create the endpoint as a specific type like: `type FindBeerById = ...`
You will have to "Capture" the provided id in the path parameters. You can find documentation and example [here](https://hackage.haskell.org/package/servant-0.7.1/docs/Servant-API-Capture.html).

This endpoint may return a beer if it exists in the list but it can also return `Nothing`. What should be the output type of this endpoint?

- Create the associated handler function which will take the `searchedId` in parameter.

- Add the endpoint to `ApplicationApi` and get the code compiling

**Expected result:** 
- If the beer exist, it should return it
- Else, it should return `Nothing` which is encoded to `null` in Json

### Step 4 - JSON to Model (Aeson - decode)

In order to be able to create new beers using a Json body, we need to define the way the beers will be decoded from JSON. 

- First, you need to uncomment the remaining tests in [test/Model/BeerSpec.hs](test/Model/BeerSpec.hs) 
- Run the tests: `stack test --file-watch`
- Then you can update [src/Model/Beer.hs](src/Model/Beer.hs) to fix the tests

**Expected result:** the tests should pass.

### Step 5 - Draft your beer (`POST`)

So you'd like to draft your own beer. To do so, we will need to create it by requesting to `POST /beers` passing the beer as Json request body like below: 

```json
{
  "id": 1,
  "name": "Brewdog IPA",
  "alcohol": 5.4,
  "style": "INDIA_PALE_ALE"
}
```

- First, you need to create the endpoint as before: 
    - You will need to provide a request body as Json of Beer model (`ReqBody '[ JSON] Beer`)
    - The request will return nothing so you can use `PostNoContent` and `NoContent` as payload
- Then you can add this new endpoint to the `ApplicationApi` definition to expose it 
- You will have to fix the compilation issue as there is no handler for this new endpoint so you need to create it
    - It will take the beer as parameter
    - It will return `NoContent`

The handler will have to deal with saving the provided beer. To do so, you can use: `insert $ toRow beer` which will return the key of the saved entity so you can bind it to a variable.

You can then use `liftIO . putStrLn $ "Saved beer with key = " <> show beerId` to log the saved beer to the console.

- What happens if you try to create a malformed Json document? 

**Expected result:** When creating a Beer as following, it should work. 
```
$ curl --request POST \
   --url http://localhost:3000/beers \
   --header 'content-type: application/json' \
   --cookie JSESSIONID=8BE347DF4D94AE3FFE227E97113825B6 \
   --data '{
   "alcohol": 5.4,
   "name": "Brewdog IPA 2",
   "style": "INDIA_PALE_ALE"
 }'
```

### Step 6 - Get all the beers again!

Now we persist beers in a memory DB, we can refactor the handler which returns all the beers so it fetches them form the DB instead of the [src/Constants.hs file](src/Constants.hs).  

Update `beersHandler` function so it fetches all the beers from the DB. 

You can use the following statement to retrieve all the `Beers` from the database. 
```haskell
do
  beersFromDB :: [Entity BeerRow] <- selectList [] []
  ... 
```
Note that `selectList` takes 2 parameters: 
- a list of filters (= `where` clauses)
- a list of `SelectOps` (like `limit`)

Then, you need to transform this list of `Entity BeerRow` to a list of `Beer` so you can use function `DatabaseStuff#fromRow`: 
```haskell
fromRow :: Entity BeerRow -> Beer
```

Tip: use `fmap` to iterate over `[Entity BeerRow]` so you can apply the transformation.

**Expected result:** After creating some beers, requesting all should return all of them:
```
$ curl --request GET --url http://localhost:3000/beers
[{"style":"INDIA_PALE_ALE","alcohol":5.4,"name":"Brewdog IPA","id":1},{"style":"INDIA_PALE_ALE","alcohol":6,"name":"Anosteke IPA","id":2}]
```

### Step 7 - Pimp it (`PUT`)

You can now create and find beers but how would we upgrade a beer?

- Create a `PUT /beers/{$beerId}` endpoint with `Beer` as a Json payload. 
- As before, add this endpoint to `ApplicationApi` and create the handler so it will update the beer in database whose id is the provided `beerId` from the path. You can use function `replace` from persistent: 
```haskell
replace :: (MonadIO m, PersistRecordBackend record backend)
            => Key record -> record -> ReaderT backend m ()
```

**Expected result:** After creating a beer, try updating its name and find it to check it has been updated.

Bonus: This function will not fail if the record does not exist. What can you do to throw an exception when we try to update a beer which does not exist?  

### Step 8 - Drink it (`DELETE`)
### Step 9 - Swagg'it (Swagger)
