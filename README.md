# RECENT CHANGES

- 06/4 - Added points to the state
- 06/4 - Forced the printing of both hand and points.
- 13/4 - Inserted some stuff here and there that we may need maybe not. See changes in the files Parser, Eval and StateMonad
- 20/4 - Now the necessary helper methods from ass. 6 + 7 have been implemented
- 20/4 - State has been updated to include playerturn, num of players, and the list of players who have forfeited. This in reflected in the RCM's in Scrabble.fs

# TODO

- Change Scrabble.fs so that
  - When tiles are placed on the board, the state needs to update the board, so that the computer
    knows where the different tiles are and can use that information to write new words. A TA suggested having a Map<coord, tile> or something like that as part of the state
  - We "parse" the different possible Gameplay Errors (RGPE), see the full list in the Scrabble.pdf document.
- Make the algorithm that makes moves when it's our turn.
- We should clean up duplicate type definitions across different files.

# Functional_Scwabble

Whenever you push a new version of your bot, increase the version number 1.0.0 to 1.0.1 , and so on. If
you do not, you will get a conflict when you try to push!

Go to the ScrabbleBot/ScrabbleBot.fsproj file and change ONLY the version number. Save the file.

```
cd ScrabbleTemplate
dotnet run ScrabbleTemplate/Program
cd..
```

This should generate a new, updated Scwabble file in the ScrabbleBot/Bin/Debug

Then run:

```
dotnet nuget push ScrabbleBot/bin/Debug/Scwabble.<VERSION-NUMBER>.nupkg --source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key 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
```

You should have now pushed succesfully.

# Other stuff

```
dotnet nuget add source https://nuget.pkg.github.com/jesper-bengtson/index.json -n FP2022 -u jesper-bengtson -p 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 --store-password-in-clear-text
dotnet nuget remove source FP2022
```
