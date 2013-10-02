#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("IntelliFactory.WebSharper.O3D", "2.5")
        .References(fun r -> [r.Assembly "System.Web"])

let main =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.O3D")
        .Embed(["o3d.js"])
        .SourcesFromProject()

let test =
    bt.WebSharper.HtmlWebsite("IntelliFactory.WebSharper.O3D.Tests")
        .SourcesFromProject()
        .References(fun r -> [r.Project main])

bt.Solution [
    main
    test

    bt.NuGet.CreatePackage()
        .Description("WebSharper Extensions for O3D 20100829")
        .ProjectUrl("https://github.com/intellifactory/websharper.glmatrix")
        .Add(main)

]
|> bt.Dispatch
