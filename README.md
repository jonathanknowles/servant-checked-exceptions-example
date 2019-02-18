## Contents

1. [Introduction](#introduction)
2. [Building](#building)
3. [Running the server](#running-the-server)
4. [Running the client](#running-the-client)
5. [Generating Swagger](#generating-swagger)

## Introduction

This repository contains a simple example of how to use the
[`servant-checked-exceptions`](https://hackage.haskell.org/package/servant-checked-exceptions)
library.

The example has a simple [API](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs),
representing a simple database of [locations](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs#L62),
where each location has a [unique integer ID](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs#L63)
and a [unique name](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs#L64).

The API provides operations to [add new locations](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs#L40),
as well as to [find existing locations](https://github.com/jonathanknowles/servant-checked-exceptions-example/blob/master/common/Api.hs#L48)
based on ther IDs or names.

Each endpoint is capable of throwing exceptions, as declared with the
[`Throws`](https://hackage.haskell.org/package/servant-checked-exceptions-core/docs/Servant-Checked-Exceptions-Internal-Servant-API.html#t:Throws)
combinator. Each exception type corresponds to a specific HTTP error code.

## Building

```Haskell
$ stack build
```

## Running the server

```Haskell
$ stack exec server
```

## Running the client

### Adding locations

```Haskell
$ stack exec client -- addLocation "Cambridge"
Location {locationId = 0, locationName = "Cambridge"}

$ stack exec client -- addLocation "Oxford"
Location {locationId = 1, locationName = "Oxford"}

$ stack exec client -- addLocation "Boston"
Location {locationId = 2, locationName = "Boston"}

$ stack exec client -- addLocation "Taipei"
Location {locationId = 3, locationName = "Taipei"}
```

### Adding locations (unsuccessfully, triggering exceptions)

```Haskell
$ stack exec client -- addLocation "A"
LocationNameTooShortError

$ stack exec client -- addLocation "****"
LocationNameHasInvalidCharsError
```

### Finding locations

```Haskell
$ stack exec client -- findLocationById 0
Location {locationId = 0, locationName = "Cambridge"}

$ stack exec client -- findLocationByName Oxford
Location {locationId = 1, locationName = "Oxford"}
```

### Finding locations (unsuccessfully, triggering exceptions)

```Haskell
$ stack exec client -- findLocationById 1000
NoMatchingLocationError

$ stack exec client -- findLocationByName Utopia
NoMatchingLocationError
```

## Generating Swagger

```Haskell
$ stack exec swagger | json_pp | less
```

