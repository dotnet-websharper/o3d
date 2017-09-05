#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.O3D")
        .VersionFrom("WebSharper")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let main =
    bt.WebSharper4.Extension("WebSharper.O3D")
        .Embed(["o3d.js"])
        .SourcesFromProject()

let test =
    bt.WebSharper4.HtmlWebsite("WebSharper.O3D.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project main
                r.NuGet("WebSharper.Html").Latest(true).ForceFoundVersion().Reference()
            ])

bt.Solution [
    main
    test

    bt.NuGet.CreatePackage()
        .Configure(fun c ->
            { c with
                Title = Some "WebSharper.O3D-20100829"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/websharper.o3d"
                Description = "WebSharper Extensions for O3D 20100829"
                RequiresLicenseAcceptance = true })
        .Add(main)
]
|> bt.Dispatch
