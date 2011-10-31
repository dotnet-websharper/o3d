namespace IntelliFactory.WebSharper.O3D.Tests

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.O3D
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.O3D.Tests.Pool

module Main =

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

