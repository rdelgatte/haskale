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


### Step 2 - Get all beers!
### Step 3 - Find the beer
### Step 4 - JSON to Model (Aeson - decode)
### Step 5 - Draft your beer (`POST`)
### Step 6 - Pimp it (`PUT`)
### Step 7 - Drink it (`DELETE`)
### Step 8 - Swagg'it (Swagger)
