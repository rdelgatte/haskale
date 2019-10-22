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
pingHandler :: Handler ??
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

- First, you need to comment the following tests in [test/Model/BeerSpec.hs](test/Model/BeerSpec.hs): 
```haskell
--test_jsonInverse =
--  testProperty "When encoding and decoding an beer, it returns the original beer" $ \(anyBeer :: Beer) ->
--    Just anyBeer === decode (encode anyBeer)
--
--test_jsonInjective =
--  testProperty "When encoding different beers, the JSONs are different" $ \(beer1 :: Beer) (beer2 :: Beer) ->
--    beer1 /= beer2 ==> encode beer1 =/= encode beer2
```
- Run the tests: `stack test --file-watch`
- Then you can update [src/Model/Beer.hs](src/Model/Beer.hs) to fix the tests

A tip: `encode :: ToJSON a => a -> ByteString`

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
beersHandler :: Handler [Beer]
beersHandler = ??
``` 
- Change server signature to: `server :: Server ApplicationApi` as now, it does not only handle the `ping` but the `ApplicationAPI`. 

- Update the code to get the compiler happy (you can combine handlers with `:<|>`). What happens if you change the order? Why?

- Update the function `mkApp` to return the right type

- Run the application and open your browser to check `http://localhost:3000/beers` 

### Step 3 - Find the beer
### Step 4 - JSON to Model (Aeson - decode)
### Step 5 - Draft your beer (`POST`)
### Step 6 - Pimp it (`PUT`)
### Step 7 - Drink it (`DELETE`)
### Step 8 - Swagg'it (Swagger)
