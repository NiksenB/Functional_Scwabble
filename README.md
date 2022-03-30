# Functional_Scwabble
Go to the ScrabbleBot/ScrabbleBot.fsproj file and change ONLY the version number. Save the file.

Go to the ScrabbleTemplate and run
```
dotnet run Project
```
This should generate a new, updated Scwabble file in the ScrabbleBot/Bin/Debug

Go to the ScrabbleBot folder and run the following command:
```
dotnet nuget push bin/Debug/Scwabble.<VERSION NUMBER>.nupkg --source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key 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
```