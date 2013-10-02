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
                Head = [Tags.Style [Text "
                    body {
                        margin:0;
                        padding:0;
                        overflow:hidden;
                    }
                    .caption {
                        position:absolute;
                        top:10px;
                        right:10px;
                        text-align:right;
                        width:300px;
                        color:white;
                        background:rgba(0,0,0,0.2);
                        padding:10px;
                    }
                    .caption p {
                        margin: 0;
                        padding: 0;
                    }"]]
                Body =
                    [
                        Div [Class "caption"] -< [
                            P [Text "Click and drag to move the view."]
                            P [Text "+/-/scroll: Zoom in / out."]
                            P [Text "spacebar: Hold down to shoot."]
                            P [Text "asdw: Position the cue ball."]
                            P [Text "t: Table view mode."]
                            P [Text "c: Cue ball view mode."]
                            P [Text "*: Rack for 8-ball."]
                            P [Text "(: Rack for 9-ball."]
                        ]
                        Div [new Samples()]
                    ] }

    let Main = Sitelet.Content "/" Index HomePage

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Action.Index]

[<assembly: Website(typeof<Website>)>]
do ()
