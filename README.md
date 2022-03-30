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