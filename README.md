## Introduction

A simple example of how to use the `servant-checked-exceptions` library.

This example has a simple API, representing a simple database of locations, where each location has a unique integer ID and a unique name.

The API provides operations to add new locations, and to find existing locations based on ther IDs or names.

Each endpoint is capable of throwing exceptions, as declared with the `Throws` combinator. Each exception type corresponds to a specific HTTP error code.

## Building
```
$ stack build
```
## Running the server
```
$ stack exec server
```
## Running the client
### Adding locations
```
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
```
$ stack exec client -- addLocation "A"
LocationNameTooShortError
$ stack exec client -- addLocation "****"
LocationNameHasInvalidCharsError
```
### Finding locations
```
$ stack exec client -- findLocationById 0
Location {locationId = 0, locationName = "Cambridge"}
$ stack exec client -- findLocationByName Oxford
Location {locationId = 1, locationName = "Oxford"}
```
### Finding locations (unsuccessfully, triggering exceptions)
```
$ stack exec client -- findLocationById 1000
NoMatchingLocationError
$ stack exec client -- findLocationByName Utopia
NoMatchingLocationError
```
