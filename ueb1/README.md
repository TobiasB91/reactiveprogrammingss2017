# Übung 1

Das hier ist die Vorlag für Aufgabenblatt 1. Wir empfehlen, die Aufgaben in 
ihrer Reihenfolge zu bearbeiten.

## Aufgabe 1

Um den Server zu bauen und zu starten benötigt ihr das Haskell build tool [stack](https://docs.haskellstack.org/en/stable/README/).

Beim ersten mal muss wahrscheinlich der GHC in der korrekten Version nachgeladen werden. Das könnt ihr mit dem Befehl

    > stack setup
  
veranlassen. Danach könnt ihr den Server bauen und ausführen (Unter Windows die beiden Befehle einfach nacheinander ausführen (ohne `&&`)):

    > stack build && stack exec server

## Aufgabe 2

Wenn ihr Aufgabe 1 gelöst habt und ein Labyrinth erzeugt habt, könnt ihr euch daran machen den in Scala geschriebenen Client entsprechend der Aufgabe anzupassen.
Startet dafür den Haskell server und lasst ihn laufen (wie oben beschrieben). Dann könnt
ihr in das Verzeichnis client wechseln und die Scala Quellen neu kompilieren:

    > cd client
    > sbt fastOptJS

Diese befehl kopiert die kompilierten JavaScript Dateien automatisch in das vom Haskell server ausgelieferte Verzeichnis (`client/assets`). Dadurch sollten die Änderungen im Browser zu sehen sein, wenn ihr die Seite neu ladet (F5 bzw. Strg+R). (Das Cacheing im Haskell Server ist deaktiviert. Solltet ihr dennoch Probleme haben könnt ihr in den Entwicklertools eures Browsers zeitweise das Cacheing ausschalten)