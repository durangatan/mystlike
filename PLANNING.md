## Globals

On every screen you can have

- 0 to 8 arrows that, when interacted with, change to the view stored in their id.
- a popup for your inventory and equipment
- a dialog (large or small)

global state wi

# Scene

a screen needs to store:

- the arrows that are active.
- the background file

game structure

- tutorial area where you learn the mechanics (key items and equipment and knowledge)
- game opens up with branching paths (an up path to the "castle" and a down path to the "swamp")
- you need to take the down path to get an item you need for the up path, and vice versa.
- once you "ring both bells", you're back in a narrow goal-based "dungeon".
- the game branches out again, this time to four branches (north east west south). You may think you can do them in any order but actually you have to do them in a semi-specific order to get the items you need.
- the final challenge dungeon of the game is narrow again.

mechanics
Key items - keys that open doors, etc. Automatically used when needed.
equipment - creates passive effects. only one can be equipped at a time.
knowledge - passive effects that are permanently gained once discovered.