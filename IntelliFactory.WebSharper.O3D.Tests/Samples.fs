namespace IntelliFactory.WebSharper.O3D.Tests

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.O3D
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.O3D.Tests.Pool

module Main =

    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let Samples () =
        let div = Div [Attr.Style "width:100%; height: 100%;"; Attr.Id "o3d"]
        let pool = new Pool()
        div
        |>! OnAfterRender (fun d ->
            pool.InitClient()
        )

type Samples() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        Main.Samples() :> _


open IntelliFactory.WebSharper.Sitelets

type Action = | Index

module Site =

    open IntelliFactory.Html

    let HomePage =
        Content.PageContent <| fun ctx ->
            { Page.Default with
                Title = Some "WebSharper O3D Tests"
                Body = [Div [new Samples()]] }

    let Main = Sitelet.Content "/" Index HomePage

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Action.Index]

[<assembly: Website(typeof<Website>)>]
do ()
