# Titel: **Final-lambda**

## Ein Top-down J-RPG mit rouge lite elementen

Cabal Quickstart: https://katychuang.com/cabal-guide/

Beschreibung:
Ein klassisches J-RPG, welches die Reise eines "nichts" zum großen Helden darstellt...
-> Inspiration: Zelda / Final Fantasy

ca. 3 "overworld" maps mit:

- Dorf /Stadt (Händler/Trainer/Quest)
- Spawnpoint (Story-Start, Quest)
- NPC-Gegner
- Optional: 1 End-Boss Map

Durch Portale betretbare Underworld:
- prozedural Generierter Content (ev. mit rogue-like Elementen)
- Co-Op Gameplay

Team: Thomas Meyer, David Schmider, Paul Felkner

Vertiefende-Themen:
- Graphic (verm. Gloss, ...)
- Linsen: zum Handling des Game-States 
- Database: Damit die Spieler das Spiel abspeichern können

- prozedural Generierter Content (ev. mit rogue-like Elementen)
- Co-Op Gameplay

### Useful Commands

- cabal install gloss --flags="GLFW -GLUT"
- cabal repl (ghci in cabal environment)
- cabal build
- cabal run
- cabal repl --ghc-options="-XTypeOperators -XDataKinds -XTypeApplications" (needed because of the delaunay stuff)

- Notes:
  - --ghc-options="-XTypeOperators -XDataKinds -XTypeApplications" is needed for all of them
