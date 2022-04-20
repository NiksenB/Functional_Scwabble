## Todo list
- [ ] mkBoard i 6eren
- [ ] mkBoard i forhold 7eren

# RECENT CHANGES

- 06/4 - Added points to the state
- 06/4 - Forced the printing of both hand and points.
- 13/4 - Inserted some stuff here and there that we may need maybe not. Search for "LOOK HERE" to see changes in the files Parser, Eval and StateMonad

# WHAT NIKOLINE HAS LEARNED

- 08/4 - There is some inconsistency in method names in Scrabble Template here and there. Freja + Jacob should soon be uploading a version on Github that shows these changes.
- 08/4 - The bot needs a boardProg function. We can use our parser from Assignment 7 for this.
- 08/4 - "board" (located in Scrabble.Util) is parsed via. the functions that we wrote in assignment 7. We'll need them, also red ones.
- 13/4 - boardprog metod (red, ass. 7) needs stmntToSquareFun metod (red, 6.12), which needs stmntEval + declare (yellow, ass. 6). Since Mai has ass. 6 yellow, what we need to write is ass. 6 and 7 red exercises.

# TODO

- We need to write red ass 6 (i don't know if we need everything, tho..)
- We need to write red ass 7 (i don't know if we need everything, tho..)
- We should clean up duplicate type definitions across different files.
- Change Scrabble.fs (start with the things written just below? :) )

- When tiles are placed on the board, the state needs to update the board, so that the computer
  knows where the different tiles are and can use that information to write new words.
  - 'mkBoard' (located in the bottom of the Parser file) may need to be implemented to achieve this?
    But that method depends on other methods/types and it's all in all very confusing. It may be
    necessary to look in Assignment 6 and 7.
  - It may be necessary to look into / implement DSL (e.g. trippleWordScore or something). Mentioned in  
    assignments 6 and used in 7
- Update state so that it has information about numbers of players and player turn, which players have forfeited the game + all tiles on the board where words can be started.

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
