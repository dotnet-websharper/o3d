module IntelliFactory.WebSharper.O3D.Samples.Pool

type Ball = {
    mutable mass : float
    mutable angularInertia : float
    mutable center : float[]
    mutable velocity : float[]
    mutable verticalAcceleration : float
    mutable orientation : float[]
    mutable angularVelocity : float[]
    mutable active : bool
    mutable sunkInPocket : int
}
with
    static member Create() = {
        mass = 1.0
        angularInertia = 0.4
        center = [|0.; 0.; 0.|]
        velocity = [|0.; 0.; 0.|]
        verticalAcceleration = 0.
        orientation = [|0.; 0.; 0.; 1.|]
        angularVelocity = [|0.; 0.; 0.|]
        active = true
        sunkInPocket = -1
    }

type Pool() =

    let record = [||]

    let mutable speedFactor = 0.
    let maxSpeed = 1.

    let balls = Array.init 16 (fun i -> Ball.Create())

    // The cue ball is slightly heavier than the other ones
    static let cueWeightRatio = 6. / 5.5
    do  balls.[0].mass <- balls.[0].mass * cueWeightRatio
        balls.[0].angularInertia <- balls.[0].angularInertia * cueWeightRatio

    let ballTransforms = Array.init 16 (fun i ->
        let transform = ()//TODO
        transform)

    let mutable walls = [||]
    let mutable collisions = [||]
    let mutable callCollisions = [||]

    member pool.placeBall(i, p, q) =
        balls.[i].center <- p
        ballTransforms.[i]//TODO

    member pool.placeBall(i, p) =
        pool.placeBall(i, p, [|0.; 0.; 0.; 1.|])

    member pool.placeBalls() =
        for i = 0 to 15 do
            let ball = balls.[i]
            if ball.active then
                pool.placeBall(i, ball.center, ball.orientation)
            else
                () // TODO
