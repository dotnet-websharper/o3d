// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace WebSharper.O3D.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.O3D
open WebSharper.O3D.Tests.Pool

module Main =

    open WebSharper.Html.Client

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


open WebSharper.Sitelets

type Action = | Index

module Site =

    open WebSharper.Html.Server

    let HomePage ctx =
        Content.Page(
            Title = "WebSharper O3D Tests",
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
                }"]],
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
                ]
        )

    let Main = Sitelet.Content "/" Index HomePage

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Action.Index]

[<assembly: Website(typeof<Website>)>]
do ()
