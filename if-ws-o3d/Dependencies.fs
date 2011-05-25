// $begin{copyright}
//
// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2010.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

namespace IntelliFactory.WebSharper.Bing.Dependencies

open IntelliFactory.WebSharper
module R = IntelliFactory.WebSharper.Resources

/// Requires the Bing Maps API.
type MapsAPI() =
    interface R.IResourceDefinition with
        member this.Resource =
            {
                Id = "MS.Bing.Maps"
                Dependencies = []
                Body =
                    R.ScriptBody <| R.ExternalLocation [
                        R.ConfigurablePart (
                            "MS.Bing.Maps",
                            "http://ecn.dev.virtualearth.net/mapcontrol/mapcontrol.ashx?v=6.3"
                        )
                    ]
            }

[<assembly: Require(typeof<MapsAPI>)>]
do ()
