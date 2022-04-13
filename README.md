# RECENT CHANGES

- Added points to the state
- Forced the printing of both hand and points.

# WHAT NIKOLINE HAS LEARNED

- There is a newer version of Scrabble Template. See Github update from Freja + Jacob, they'll be uploading soon.
- The bot needs a boardProg function. We can use our parser from Assignment 7 for this.
- "board" (located in Scrabble.Util) is parsed via. the functions that we wrote in assignment 7. We'll need them, also red ones.

# TODO

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
