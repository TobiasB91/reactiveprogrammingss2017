# Übung 4: Curried in Space

Das hier ist die Vorlage für Aufgabenblatt 4.

## Haskell Server:

Die Quellen liegen unter `haskell-server`

Bauen mit:

```sh
stack setup # nur beim ersten mall
stack build # übersetzen
stack exec curried-in-space # server starten
```

## Scala Server:

Die Quellen liegen unter `scala-server`

Bauen und ausführen mit (Client wird automatisch mit übersetzt):

```sh
sbt run # oder sbt ~run für continuous build
```

Server Stoppen mit Enter

## Client

Die quellen liegen in  `scala-client`.

Übersetzen von Scala nach JavaScript (Aus dem root verzeichnis):

```sh
sbt
> project client
> fastOptJS
```