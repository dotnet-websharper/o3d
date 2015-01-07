module IntelliFactory.WebSharper.O3D.Tests.Pool

open IntelliFactory.WebSharper.O3D
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript

type Float2 = float * float
type Float3 = float * float * float
type Float4 = float * float * float * float
type Mat3 = Float3 * Float3 * Float3

type Ball = {
    mutable Mass : float
    mutable AngularInertia : float
    mutable Center : Float3
    mutable Velocity : Float3
    mutable VerticalAcceleration : float
    mutable Orientation : Float4
    mutable AngularVelocity : Float3
    mutable Active : bool
    mutable SunkInPocket : int
}
with
    [<JavaScript>]
    static member Create() = {
        Mass = 1.0
        AngularInertia = 0.4
        Center = (0., 0., 0.)
        Velocity = (0., 0., 0.)
        VerticalAcceleration = 0.
        Orientation = (0., 0., 0., 1.)
        AngularVelocity = (0., 0., 0.)
        Active = true
        SunkInPocket = -1
    }

type BallCollision = {
    i : int
    j : int
    ammt : float
}

type WallCollision = {
    i : int
    x : float
    y : float
    ammt : float
}

type Wall = {
    p : Float2
    q : Float2
    nx : float
    ny : float
    k : float
    a : float
    b : float
}
with
    [<JavaScript>]
    static member ComputeNormals(arr) =
        arr |> Array.map (fun ((px, py as p) : Float2, (qx, qy as q) : Float2) ->
            let (tx, ty) = O3DJS.Math.Normalize(O3DJS.Math.Sub(q, p))
            let nx = ty
            let ny = -tx
            { p = p; q = q; nx = nx; ny = ny
              k = nx * px + ny * py
              a = py * nx - px * ny
              b = qy * nx - qx * ny })

type CameraPosition = {
    mutable Center : Float3
    mutable Theta : float
    mutable Phi : float
    mutable Radius : float
}
with
    [<JavaScript>]
    static member Create() =
        { Center = (0., 0., 0.)
          Theta = 0.
          Phi = 0.
          Radius = 1. }

type QueueElement = {
    condition : float -> bool // clock -> bool
    action : unit -> unit
}

[<JavaScript>]
let mutable g_clock = 0.
[<JavaScript>]
let mutable g_queueClock = 0.
[<JavaScript>]
let mutable g_shadowOnParams = [||] : O3D.Param<float>[]

type Shot = {
    Target : Float2
    Power : float
    Difficulty : float
}
with
    [<JavaScript>]
    static member Null =
        { Target = (0., 0.)
          Power = 0.
          Difficulty = infinity }

type CameraInfo = {
    mutable lastX : float
    mutable lastY : float
    mutable position : CameraPosition
    targetPosition : CameraPosition
    vector_ : Float3
    mutable lerpCoefficient : float
    mutable startingTime : float
}
with
    [<JavaScript>]
    member this.Begin(x, y) =
        this.lastX <- x
        this.lastY <- y

    [<JavaScript>]
    member this.Update(x, y) =
        this.targetPosition.Theta <- this.targetPosition.Theta - (x - this.lastX) / 200.
        this.targetPosition.Phi <- this.targetPosition.Phi + (y - this.lastY) / 200.
        this.Bound()
        this.lastX <- x
        this.lastY <- y

    [<JavaScript>]
    member this.Bound() =
        if this.position.Phi < 0.01 then
            this.position.Phi <- 0.01
        if this.position.Phi > System.Math.PI / 2. - 0.01 then
            this.position.Phi <- System.Math.PI / 2. - 0.01

    [<JavaScript>]
    member this.GetCurrentPosition() =
        let t = this.lerpCoefficient
        let t = 3. * t * t - 2. * t * t * t
        let a = this.position
        let b = this.targetPosition
        { Center = O3DJS.Math.Add(O3DJS.Math.Mul(1. - t, a.Center),
                                  O3DJS.Math.Mul(t, b.Center))
          Radius = (1. - t) * a.Radius + t * b.Radius
          Theta = (1. - t) * a.Theta + t * b.Theta
          Phi = (1. - t) * a.Phi + t * b.Phi }

    [<JavaScript>]
    member this.GetEyeAndTarget() =
        let p = this.GetCurrentPosition()
        let cosPhi = Math.Cos p.Phi
        let target = p.Center
        let eye = O3DJS.Math.Add(target, O3DJS.Math.Mul(p.Radius, (cosPhi * Math.Cos p.Theta,
                                                                   cosPhi * Math.Sin p.Theta,
                                                                   Math.Sin p.Phi)))
        (eye, target)

    [<JavaScript>]
    member this.GoTo(center, theta, phi, radius) =
        let center = defaultArg center this.targetPosition.Center
        let theta = defaultArg theta this.targetPosition.Theta
        let phi = defaultArg phi this.targetPosition.Phi
        let radius = defaultArg radius this.targetPosition.Radius
        let p = this.GetCurrentPosition()
        this.position <- p
        this.targetPosition.Center <- center
        this.targetPosition.Theta <- theta
        this.targetPosition.Phi <- phi
        this.targetPosition.Radius <- radius
        this.lerpCoefficient <- 0.
        this.startingTime <- g_clock
        let k = 3. * System.Math.PI / 2.
        let myMod n m = ((n % m) + m) % m
        this.position.Theta <-
            myMod (this.position.Theta + k) (2.*System.Math.PI) - k
        this.targetPosition.Theta <-
            myMod (this.targetPosition.Theta + k) (2.*System.Math.PI) - k

    [<JavaScript>]
    member this.BackUp() =
        let c = this.targetPosition.Center
        this.GoTo(Some c, None, Some (System.Math.PI/6.), Some 100.)

    [<JavaScript>]
    member this.ZoomToPoint(center) =
        this.GoTo(Some center, Some this.targetPosition.Theta, Some (System.Math.PI/20.), Some 20.)

    [<JavaScript>]
    member this.UpdateClock() =
        this.lerpCoefficient <- min 1. (g_clock - this.startingTime)
        if this.lerpCoefficient = 1. then
            this.position.Center <- this.targetPosition.Center
            this.position.Theta <- this.targetPosition.Theta
            this.position.Phi <- this.targetPosition.Phi
            this.position.Radius <- this.targetPosition.Radius

    [<JavaScript>]
    member this.LookingAt(center) =
        this.targetPosition.Center = center

    [<JavaScript>]
    static member Create() =
        let c = { lastX = 0.
                  lastY = 0.
                  position = CameraPosition.Create()
                  targetPosition = CameraPosition.Create()
                  vector_ = (0., 0., 0.)
                  lerpCoefficient = 1.
                  startingTime = 0. }
        c.GoTo(Some (0., 0., 0.),
               Some (- System.Math.PI / 2.),
               Some (System.Math.PI / 6.),
               Some 140.)
        c

[<JavaScript>]
let g_tableWidth = 45.
[<JavaScript>]
let g_pocketRadius = 2.3
[<JavaScript>]
let g_woodBreadth = 3.2
[<JavaScript>]
let g_tableThickness = 5.
[<JavaScript>]
let g_woodHeight = 1.1
[<JavaScript>]
let g_ballTransforms = Array.init 16 (fun i -> null : O3D.Transform)
[<JavaScript>]
let mutable g_centers = [||] : O3D.Param<Float2>[]

type Physics[<JavaScript>]() =

    [<JavaScript>]
    let record = [||]

    [<JavaScript>]
    let mutable speedFactor = 0.
    [<JavaScript>]
    let maxSpeed = 1.

    [<JavaScript>]
    let pocketCenters =
        let w = g_tableWidth / 2.
        let r = g_pocketRadius
        let root2 = Math.Sqrt 2.
        let x = 0.5 * root2 * r - w
        let y = 0.5 * root2 * r - 2. * w
        [| (w, 0.); (-w, 0.); (x, y); (-x, y); (x, -y); (-x, -y) |]
    [<JavaScript>]
    let left = -g_tableWidth / 2. + g_pocketRadius + 1.
    [<JavaScript>]
    let right = g_tableWidth / 2. - g_pocketRadius - 1.
    [<JavaScript>]
    let top = g_tableWidth - g_pocketRadius - 1.
    [<JavaScript>]
    let bottom = - g_tableWidth + g_pocketRadius + 1.

    // The cue ball is slightly heavier than the other ones
    [<JavaScript>]
    let cueWeightRatio = 6. / 5.5
    [<JavaScript>]
    let balls = Array.init 16 (fun i ->
        let ball = Ball.Create()
        if i = 0 then
            ball.Mass <- ball.Mass * cueWeightRatio
            ball.AngularInertia <- ball.AngularInertia * cueWeightRatio
        ball)

    [<JavaScript>]
    let mutable walls = [||]
    [<JavaScript>]
    let mutable collisions = []
    [<JavaScript>]
    let mutable wallCollisions = []

    [<JavaScript>]
    let vectorToQuaternion ((r0, r1, r2) as r, ()) =
        let theta = O3DJS.Math.Length r
        let stot = if theta < 1.0e-6 then 1. else Math.Sin(theta/2.) / theta
        (stot * r0, stot * r1, stot * r2, Math.Cos(theta))

    [<JavaScript>]
    member this.Balls = balls
    [<JavaScript>]
    member this.PocketCenters = pocketCenters
    [<JavaScript>]
    member this.SpeedFactor
        with get() = speedFactor
        and set value = speedFactor <- value

    [<JavaScript>]
    member this.BallOn i =
        this.Balls.[i].Active <- true
        this.Balls.[i].SunkInPocket <- -1
        g_ballTransforms.[i].Visible <- true
        g_shadowOnParams.[i].Value <- 1.

    [<JavaScript>]
    member this.BallOff i =
        this.Balls.[i].Active <- false
        g_ballTransforms.[i].Visible <- false
        g_shadowOnParams.[i].Value <- 0.

    [<JavaScript>]
    member this.PlaceBall(i, ((x, y, _) as p), q) =
        balls.[i].Center <- p
        g_ballTransforms.[i].LocalMatrix <- O3DJS.Math.Matrix4.Translation p
        g_ballTransforms.[i].QuaternionRotate q
        g_centers.[i].Value <- (x, y)

    [<JavaScript>]
    member this.PlaceBall(i, p) =
        this.PlaceBall(i, p, (0., 0., 0., 1.))

    [<JavaScript>]
    member this.PlaceBalls() =
        balls |> Array.iteri (fun i ball ->
            if ball.Active then
                this.PlaceBall(i, ball.Center, ball.Orientation)
            else
                g_shadowOnParams.[i].Value <- 0.)

    [<JavaScript>]
    member this.Step() =
        for i = 0 to 4 do
            this.BallsLoseEnergy()
            this.BallsImpactFloor()
            this.Move(1.)
            while this.Collide() do
                this.Move(-1.)
                this.HandleCollisions()
                this.Move(1.)
            this.Sink()
            this.HandleFalling()
            this.PlaceBalls()

    [<JavaScript>]
    member this.Move(timeStep : float) =
        balls |> Array.iter (fun ball ->
            if ball.Active then
                let (cx, cy, cz) = O3DJS.Math.Add(ball.Center, O3DJS.Math.Mul(timeStep, ball.Velocity))
                let vec = O3DJS.Math.Mul(timeStep, ball.AngularVelocity)
                ball.Orientation <- O3DJS.Quaternions.Mul(vectorToQuaternion(vec,()), ball.Orientation)
                                    |> O3DJS.Quaternions.Normalize
                ball.Center <- (cx, cy, cz)
                let vx, vy, vz = ball.Velocity
                ball.Velocity <- (vx, vy, vz + ball.VerticalAcceleration))

    [<JavaScript>]
    member this.ImpartSpeed(i, (dx, dy)) =
        let ball = balls.[i]
        let factor = maxSpeed * speedFactor
        let bx, by, bz = ball.Velocity
        ball.Velocity <- (bx + dx*factor, by + dy*factor, bz)

    [<JavaScript>]
    member this.StopAllBalls() =
        balls |> Array.iter (fun ball ->
            ball.Velocity <- (0., 0., 0.)
            ball.AngularVelocity <- (0., 0., 0.))

    [<JavaScript>]
    member this.StopSlowBalls() =
        let epsilon = 0.0001
        balls |> Array.iter (fun ball ->
            if ball.Active then
                if O3DJS.Math.Length ball.Velocity < epsilon then
                    ball.Velocity <- (0., 0., 0.)
                if O3DJS.Math.Length ball.AngularVelocity < epsilon then
                    ball.AngularVelocity <- (0., 0., 0.))

    [<JavaScript>]
    member this.SomeBallsMoving() =
        balls |> Array.exists (fun ball ->
            ball.Active &&
            (let (v0, v1, v2) = ball.Velocity
             let (w0, w1, w2) = ball.AngularVelocity
             v0 <> 0. || v1 <> 0. || v2 <> 0. ||
             w0 <> 0. || w1 <> 0. || w2 <> 0.))

    [<JavaScript>]
    member this.Sink() =
        balls |> Array.iter (fun ball ->
            if ball.Active then
                let (px, py, _) = ball.Center
                pocketCenters |> Array.iteri (fun j pocketCenter ->
                    if O3DJS.Math.DistanceSquared((px, py), pocketCenter) < g_pocketRadius*g_pocketRadius then
                        ball.VerticalAcceleration <- -0.005
                        ball.SunkInPocket <- j))

    [<JavaScript>]
    member this.HandleFalling() =
        balls |> Array.iteri (fun i ball ->
            if ball.Active then
                let (px, py, pz) = ball.Center
                if ball.SunkInPocket >= 0 then
                    let pcx, pcy as pocketCenter = pocketCenters.[ball.SunkInPocket]
                    // let dx = px - pcx
                    // let dy = py - pcy
                    // let norm = Math.Sqrt(dx*dx + dy*dy)
                    // let maxNorm = g_pocketRadius - Math.Sqrt(Math.Max(0., (1. - (pz + 1.) * (pz + 1.))))
                    // if norm > maxNorm then
                    //     ball.Center <- (pcx + dx * maxNorm / norm,
                    //                     pcy + dy * maxNorm / norm,
                    //                     pz)
                    let (dx, dy) as d = O3DJS.Math.Sub((px, py), pocketCenter)
                    let norm = O3DJS.Math.Length d
                    let maxNorm = g_pocketRadius - Math.Sqrt (max 0. (1. - (pz + 1.) * (pz + 1.)))
                    if norm > maxNorm then
                        let ratio = maxNorm / norm
                        ball.Center <- (pcx + dx*ratio, pcy + dy*ratio, pz)
                if pz < -3. then
                    ball.Velocity <- (0., 0., 0.)
                    ball.AngularVelocity <- (0., 0., 0.)
                    ball.VerticalAcceleration <- 0.
                    ball.Active <- false
                    this.BallOff i)

    [<JavaScript>]
    member this.BoundCueBall() =
        let mutable (cx, cy, _) = balls.[0].Center
        if cx < left then
            cx <- left
        if cx > right then
            cx <- right
        if cy < bottom then
            cy <- bottom
        if cy > top then
            cy <- top
        this.PushOut()
        this.PlaceBalls()

    [<JavaScript>]
    member this.Collide() =
        this.CollideBalls()
        this.CollideWithWalls()
        not (List.isEmpty collisions) || not (List.isEmpty wallCollisions)

    [<JavaScript>]
    member this.PushOut() =
        while this.Collide() do
            this.PushCollisions()

    [<JavaScript>]
    member this.CollideBalls() =
        collisions <- []
        balls |> Array.iteri (fun i balli ->
            if balli.Active then
                let (p1x, p1y, _) = balli.Center
                for j = 0 to i - 1 do
                    let ballj = balls.[j]
                    if ballj.Active then
                        let (p2x, p2y, _) = ballj.Center
                        let d2 = O3DJS.Math.DistanceSquared((p1x, p1y), (p2x, p2y))
                        if d2 < 3.99 then
                            let norm = Math.Sqrt d2
                            let collision = {i = i; j = j; ammt = 2. - norm}
                            collisions <- collision :: collisions)

    [<JavaScript>]
    member this.InitWalls() =
        let r = g_pocketRadius
        let w = g_tableWidth
        let path = [| (0., -w/2. +    r, 0.)
                      ( r, -w/2. + 2.*r, 0.)
                      ( r,  w/2. - 2.*r, 0.)
                      (0.,  w/2. -    r, 0.) |]
        let pi = System.Math.PI
        let angles = [| 0.; pi/2.; pi; pi; 3.*pi/2.; 0. |]
        let translations = O3DJS.Math.Mul([|[|-1.; -1.; 0.|]
                                            [| 0.; -2.; 0.|]
                                            [| 1.; -1.; 0.|]
                                            [| 1.;  1.; 0.|]
                                            [| 0.;  2.; 0.|]
                                            [|-1.;  1.; 0.|]|],
                                          [|[|w/2.;   0.; 0.|]
                                            [|  0.; w/2.; 0.|]
                                            [|  0.;   0.; 1.|]|])
        walls <-
            Array.init 6 (fun i ->
                let newPath = path |> Array.mapi (fun j p ->
                    O3DJS.Math.Matrix4.TransformPoint(
                        O3DJS.Math.Matrix4.Composition(
                            O3DJS.Math.Matrix4.Translation(translations.[i]),
                            O3DJS.Math.Matrix4.RotationZ(angles.[i])),
                        p))
                Array.init (newPath.Length - 1) (fun j ->
                    let (px, py, _) = newPath.[j]
                    let (qx, qy, _) = newPath.[j + 1]
                    (px, py), (qx, qy)))
            |> Array.concat
            |> Wall.ComputeNormals

    [<JavaScript>]
    member this.CollideWithWalls(wallList, radius) =
        List.init 16 (fun i ->
            let ball = balls.[i]
            let (x, y, _) = ball.Center
            if ball.Active && not (x > left && x < right &&
                                   y > bottom && y < top) then
                walls
                |> Seq.tryPick (fun wall ->
                    let norm = abs (x * wall.nx + y * wall.ny - wall.k)
                    if norm < radius then
                        let t = y * wall.nx - x * wall.ny
                        if t > wall.a && t < wall.b then
                            Some {i = i; x = wall.nx; y = wall.ny; ammt = 1. - norm}
                        else
                            let (dx, dy) as d = O3DJS.Math.Sub((x, y), wall.p)
                            let normSquared = O3DJS.Math.LengthSquared d
                            if normSquared < radius * radius then
                                let norm = Math.Sqrt normSquared
                                Some {i = i; x = dx / norm; y = dy / norm; ammt = 1. - norm}
                            else
                                let (dx, dy) as d = O3DJS.Math.Sub((x, y), wall.q)
                                let normSquared = O3DJS.Math.LengthSquared d
                                if normSquared < radius * radius then
                                    let norm = Math.Sqrt normSquared
                                    Some {i = i; x = dx / norm; y = dy / norm; ammt = 1. - norm}
                                else None
                    else None)
            else None)
        |> List.choose id

    [<JavaScript>]
    member this.CollideWithWalls() =
        wallCollisions <- this.CollideWithWalls(walls, 1.)

    [<JavaScript>]
    member this.PushCollisions() =
        wallCollisions |> List.iter (fun c ->
            let (p0, p1, p2) = balls.[c.i].Center
            balls.[c.i].Center <- (p0 + c.ammt*c.x, p1 + c.ammt*c.y, p2))
        collisions |> List.iter (fun c ->
            let pi = balls.[c.i].Center
            let pj = balls.[c.j].Center
            let (dx, dy, _) = O3DJS.Math.Sub(pj, pi)
            let norm = O3DJS.Math.Length((dx, dy))
            let r = (c.ammt * dx / norm / 2., c.ammt * dy / norm / 2., 0.)
            balls.[c.i].Center <- O3DJS.Math.Sub(pi, r)
            balls.[c.j].Center <- O3DJS.Math.Add(pj, r))

    [<JavaScript>]
    member this.HandleCollisions() =
        wallCollisions |> List.iter (fun c ->
            let ball = balls.[c.i]
            let v = ball.Velocity
            let w = ball.AngularVelocity
            let r1 = (-c.x, -c.y, 0.)
            let r2 = (c.x, c.y, 0.)
            let impulse = this.Impulse(v, w, ball.Mass, ball.AngularInertia, r1,
                                       (0., 0., 0.), (0., 0., 0.), 1e100, 1e100, r2,
                                       r1, 0.99, 1., 1.)
            this.ApplyImpulse(c.i, impulse, r1))
        collisions |> List.iter (fun c ->
            let bi = balls.[c.i]
            let bj = balls.[c.j]
            let vi = bi.Velocity
            let wi = bi.AngularVelocity
            let vj = bj.Velocity
            let wj = bj.AngularVelocity
            let ri = O3DJS.Math.Normalize(O3DJS.Math.Sub(bj.Center, bi.Center))
            let rj = O3DJS.Math.Negative(ri)
            let impulse = this.Impulse(vi, wi, bi.Mass, bi.AngularInertia, ri,
                                       vj, wj, bj.Mass, bj.AngularInertia, rj,
                                       ri, 0.99, 0.2, 0.1)
            this.ApplyImpulse(c.i, impulse, ri)
            this.ApplyImpulse(c.j, O3DJS.Math.Negative(impulse), rj))

    [<JavaScript>]
    member this.RandomOrientations() =
        balls |> Array.iter (fun ball ->
            ball.Orientation <- O3DJS.Math.Normalize((Math.Random() - 0.5,
                                                      Math.Random() - 0.5,
                                                      Math.Random() - 0.5,
                                                      Math.Random() - 0.5)))

    // Ensure that z = 0. because unlike the original js,
    // we use the entire vector with eg. Math.Cross
    [<JavaScript>]
    member this.Impulse(v1, w1, m1, I1, r1, v2, w2, m2, I2, r2, N, e, u_s, u_d) =
        let (x, y, _) = this.Impulse'(v1, w1, m1, I1, r1, v2, w2, m2, I2, r2, N, e, u_s, u_d)
        (x, y, 0.)

    [<JavaScript>]
    member this.Impulse'(v1, w1, m1, I1, r1,
                         v2, w2, m2, I2, r2,
                         N, e, u_s, u_d) =
        // Just to be safe, make N unit-length.
        let N = O3DJS.Math.Normalize N

        // Vr is the relative Velocity at the point of impact.
        // Vrn and Vrt are the normal and tangential parts of Vr.
        let Vr = O3DJS.Math.Sub(O3DJS.Math.Add(O3DJS.Math.Cross(w2, r2), v2),
                                O3DJS.Math.Add(O3DJS.Math.Cross(w1, r1), v1))
        let Vrn = O3DJS.Math.Mul(O3DJS.Math.Dot(Vr, N), N)
        let Vrt = O3DJS.Math.Sub(Vr, Vrn)

        let K = O3DJS.Math.Add(this.InertialTensor(m1, I1, r1), this.InertialTensor(m2, I2, r2))
        let Kinverse = O3DJS.Math.Inverse(K)

        // Compute the impulse assuming 0 tangential Velocity.
        let j0 = O3DJS.Math.Mul(Kinverse, O3DJS.Math.Sub(Vr, O3DJS.Math.Mul(-e, Vrn)))

        // If j0 is in the static friction cone, we return that.
        // If the length of Vrt is 0, then we cannot normalize it,
        // so we return j0 in that case, too.
        let j0n = O3DJS.Math.Mul(O3DJS.Math.Dot(j0, N), N)
        let j0t = O3DJS.Math.Sub(j0, j0n)

        if O3DJS.Math.LengthSquared j0t <= u_s * u_s * O3DJS.Math.LengthSquared j0n ||
           O3DJS.Math.LengthSquared Vrt = 0. then
            j0
        else
            // Get a unit-length tangent vector by normalizing the tangent Velocity.
            // The friction impulse acts in the opposite direction.
            let T = O3DJS.Math.Normalize Vrt

            // Compute the current impulse in the normal direction.
            let jn = O3DJS.Math.Dot(O3DJS.Math.Mul(Kinverse, Vr), N)

            // Compute the impulse assuming no friction.
            let js = O3DJS.Math.Mul(Kinverse,
                                    O3DJS.Math.Mul(1. + e, Vrn))

            // Return the frictionless impulse plus the impulse due to friction.
            O3DJS.Math.Add(js, O3DJS.Math.Mul(-u_d * jn, T))

    [<JavaScript>]
    member this.InertialTensor(m, I, r) : Mat3 =
        let (a, b, c) = r

        ((1. / m + (b * b + c * c) / I, (-a * b) / I, (-a * c) / I),
         ((-a * b) / I, 1. / m + (a * a + c * c) / I, (-b * c) / I),
         ((-a * c) / I, (-b * c) / I, 1. / m + (a * a + b * b) / I))

    [<JavaScript>]
    member this.ApplyImpulse(i, (ix, iy, _ as impulse) : float * float * float, r) =
        let ball = balls.[i]
        let (rx, ry, rz) = r
        ball.Velocity <- O3DJS.Math.Add(ball.Velocity, O3DJS.Math.Div(impulse, ball.Mass))
        ball.AngularVelocity <- O3DJS.Math.Add(ball.AngularVelocity,
                                               O3DJS.Math.Div(O3DJS.Math.Cross(r, impulse),
                                                              ball.AngularInertia))

    [<JavaScript>]
    member this.BallsImpactFloor() =
        balls |> Array.iteri (fun i ball ->
            if ball.Active then
                let (vx, vy, _) = ball.Velocity
                let v = (vx, vy, -0.1)
                let w = ball.AngularVelocity
                let impulse = this.Impulse(v, w, ball.Mass, ball.AngularInertia, (0., 0., -1.),
                                           (0., 0., 0.), (0., 0., 0.), 1e100, 1e100, (0., 0., 1.),
                                           (0., 0., -1.), 0.1, 0.1, 0.02)
                this.ApplyImpulse(i, impulse, (0., 0., -1.)))

    [<JavaScript>]
    member this.BallsLoseEnergy() =
        balls |> Array.iter (fun ball ->
            if ball.Active then
                ball.Velocity <- this.LoseEnergy(ball.Velocity, 0.00004)
                ball.AngularVelocity <- this.LoseEnergy(ball.AngularVelocity, 0.00006))

    [<JavaScript>]
    member this.LoseEnergy(v, epsilon) =
        let vLength = O3DJS.Math.Length v
        if vLength < epsilon then
            (0., 0., 0.)
        else
            let t = epsilon / vLength
            O3DJS.Math.Mul(v, 1. - t)

type Pool [<JavaScript>]() =

    let SHADOWPOV = false
    let RENDER_TARGET_WIDTH = 512
    let RENDER_TARGET_HEIGHT = 512
    let g_light = (0., 0., 50.)
    let mutable g_o3dElement : Dom.Element = null
    let g_cameraInfo = CameraInfo.Create()
    let mutable g_dragging = false
    let mutable g_client : O3D.Client = null
    let mutable g_pack : O3D.Pack = null
    let mutable g_tableRoot : O3D.Transform = null
    let mutable g_shadowRoot : O3D.Transform = null
    let mutable g_hudRoot : O3D.Transform = null
    let mutable g_viewInfo : O3DJS.Rendergraph.ViewInfo = null
    let mutable g_hudViewInfo : O3DJS.Rendergraph.ViewInfo = null
    let mutable g_shadowTexture : O3D.Texture2D = null
    let mutable g_shadowPassViewInfo : O3DJS.Rendergraph.ViewInfo = null
    let mutable g_materials : obj = null
    let mutable g_solidMaterial : O3D.Material = null
    let mutable g_shadowSampler : O3D.Sampler = null
    let mutable g_ballTextures = Array.init 16 (fun _ -> null : O3D.Texture2D)
    let mutable g_ballTextureSamplers = Array.init 16 (fun _ -> null : O3D.Sampler)
    let mutable g_ballTextureSamplerParams = Array.init 16 (fun _ -> null : O3D.Param<O3D.Sampler>)
    let mutable g_barScaling : O3D.Transform = null
    let mutable g_barRoot : O3D.Transform = null
    let mutable g_shooting = false
    let mutable g_rolling = false
    let mutable g_queue : QueueElement[] = [||]
    let mutable g_physics = new Physics()
    let mutable g_phi = 0.

    [<JavaScript>]
    let SetOptionalParam(material : O3D.Material, name, value : obj) =
        let param = material.GetParam name
        if As<bool> param then
            param.Value <- value

    [<JavaScript>]
    let UpdateMaterials() =
        JS.ForEach g_materials (fun name ->
            let eye, _ = g_cameraInfo.GetEyeAndTarget()
            SetOptionalParam((?) g_materials name, "eyeWorldPosition", eye)
            false)

    [<JavaScript>]
    let UpdateContext() =
        let perspective = O3DJS.Math.Matrix4.Perspective(O3DJS.Math.DegToRad 30.,
                                                         float g_client.Width / float g_client.Height,
                                                         1., 5000.)
        g_shadowPassViewInfo.DrawContext.Projection <- perspective
        g_viewInfo.DrawContext.Projection <- perspective
        let eye, target = g_cameraInfo.GetEyeAndTarget()
        let view = O3DJS.Math.Matrix4.LookAt(eye, target, (0., 0., 1.))
        g_shadowPassViewInfo.DrawContext.View <- view
        g_viewInfo.DrawContext.View <- view
        UpdateMaterials()

    [<JavaScript>]
    let StartDragging(e : O3D.Event) =
        g_cameraInfo.Begin(float e.X, float e.Y)
        g_dragging <- true

    [<JavaScript>]
    let Drag(e : O3D.Event) =
        if g_dragging then
            g_cameraInfo.Update(float e.X, float e.Y)
            UpdateContext()

    [<JavaScript>]
    let StopDragging(e : O3D.Event) =
        if g_dragging then
            g_cameraInfo.Update(float e.X, float e.Y)
            UpdateContext()
        g_dragging <- false

    [<JavaScript>]
    let InitPhysics() =
        g_physics.InitWalls()

    [<JavaScript>]
    let InitGlobals(clientElements : O3DJS.ClientElement[]) =
        g_o3dElement <- clientElements.[0]
        g_client <- clientElements.[0].Client
        g_pack <- g_client.CreatePack()

    [<JavaScript>]
    let InitRenderGraph() =
        g_tableRoot <- g_pack.CreateTransform(Parent = g_client.Root)
        g_shadowRoot <- g_pack.CreateTransform(Parent = g_client.Root)
        g_hudRoot <- g_pack.CreateTransform(Parent = g_client.Root)
        let viewRoot = g_pack.CreateRenderNode(Priority = 1.)
        if not SHADOWPOV then
            viewRoot.Parent <- g_client.RenderGraphRoot
        let shadowPassRoot = g_pack.CreateRenderNode(Priority = 0.,
                                                     Parent = g_client.RenderGraphRoot)
        g_viewInfo <- O3DJS.Rendergraph.CreateBasicView(g_pack, g_tableRoot, viewRoot, (0., 0., 0., 1.))
        let hudRenderRoot =
            if SHADOWPOV then null
                         else g_client.RenderGraphRoot
        g_hudViewInfo <- O3DJS.Rendergraph.CreateBasicView(g_pack, g_hudRoot, hudRenderRoot)
        g_hudViewInfo.Root.Priority <- g_viewInfo.Root.Priority + 1.
        g_hudViewInfo.ClearBuffer.ClearColorFlag <- false
        g_hudViewInfo.ZOrderedState.GetStateParamCullMode.Value <- O3D.State.CULL_NONE
        g_hudViewInfo.ZOrderedState.GetStateParamZWriteEnable.Value <- false
        g_hudViewInfo.DrawContext.Projection <- O3DJS.Math.Matrix4.Orthographic(0., 1., 0., 1., -10., 10.)
        g_hudViewInfo.DrawContext.View <- O3DJS.Math.Matrix4.LookAt((0., 0., 1.),
                                                                    (0., 0., 0.),
                                                                    (0., 1., 0.))
        g_shadowTexture <- g_pack.CreateTexture2D(RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT,
                                              O3D.Texture.XRGB8, 1, true)
        let renderSurface = g_shadowTexture.GetRenderSurface 0
        let depthSurface = g_pack.CreateDepthStencilSurface(RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT)
        let renderSurfaceSet = g_pack.CreateRenderSurfaceSet(RenderSurface = renderSurface,
                                                             RenderDepthStencilSurface = depthSurface,
                                                             Parent = shadowPassRoot)
        let shadowPassParent = if SHADOWPOV then g_client.RenderGraphRoot
                                            else renderSurfaceSet :> O3D.RenderNode
        g_shadowPassViewInfo <- O3DJS.Rendergraph.CreateBasicView(g_pack, g_shadowRoot, shadowPassParent, (0., 0., 0., 1.))
        g_shadowPassViewInfo.ZOrderedState.GetStateParamZComparisonFunction.Value <- O3D.State.CMP_ALWAYS

    [<JavaScript>]
    let HandleResizeEvent event =
        UpdateContext()

    let VertexShaderString = "
        uniform mat4 worldViewProjection;
        uniform mat4 worldInverseTranspose;
        uniform mat4 world;

        attribute vec4 position;
        attribute vec3 normal;

        varying vec4 vposition;
        varying vec4 vobjectPosition;
        varying vec3 vworldPosition;
        varying vec4 vscreenPosition;
        varying vec3 vnormal;

        void main() {
        vposition = worldViewProjection * position;
        vec4 temp = vposition;
        temp += temp.w * vec4(1.0, 1.0, 0.0, 0.0);
        temp.xyz /= 2.0;
        vscreenPosition = temp;
        vnormal = (worldInverseTranspose * vec4(normal, 0.0)).xyz;
        vworldPosition = (world * vec4(position.xyz, 1.0)).xyz;
        vobjectPosition = position;
        gl_Position = vposition;
        }"

    let PixelShaderString = "
        uniform vec3 lightWorldPosition;
        uniform vec3 eyeWorldPosition;
        uniform float factor;
        uniform float shadowOn;

        uniform sampler2D textureSampler;

        uniform vec2 ballCenter;

        varying vec4 vposition;
        varying vec4 vobjectPosition;
        varying vec3 vworldPosition;
        varying vec4 vscreenPosition;
        varying vec3 vnormal;

        vec4 roomColor(vec3 p, vec3 r) {
        vec2 c = vec2(1.0 / 15.0, 1.0 / 30.0) *
            (p.xy + r.xy * (lightWorldPosition.z - p.z) / r.z);

        float temp = (abs(c.x + c.y) + abs(c.y - c.x));
        float t = min(0.15 * max(7.0 - temp, 0.0) +
                    ((temp < 5.0) ? 1.0 : 0.0), 1.0);
        return vec4(t, t, t, 1.0);
        }

        vec4 lighting(vec4 pigment, float shininess) {
        vec3 p = vworldPosition;
        vec3 l = normalize(lightWorldPosition - p);  // Toward light.
        vec3 n = normalize(vnormal);                 // Normal.
        vec3 v = normalize(eyeWorldPosition - p);    // Toward eye.
        vec3 r = normalize(-reflect(v, n));          // Reflection of v across n.

        // return vec4(n, 1.);

        return vec4(max(dot(l, n), 0.0) * pigment.xyz +
            0.2 * pow(max(dot(l, r), 0.0), shininess) * vec3(1, 1, 1), 1.0);
        }

        vec4 woodPigment(vec3 p) {
        vec3 core = normalize(
            (abs(p.y) > abs(p.x) + 1.0) ?
                vec3(1.0, 0.2, 0.3) : vec3(0.2, 1.0, 0.3));
        float grainThickness = 0.02;
        float t =
            mod(length(p - dot(p,core)*core), grainThickness) / grainThickness;

        return mix(vec4(0.15, 0.05, 0.0, 0.1), vec4(0.1, 0.0, 0.0, 0.1), t);
        }

        vec4 feltPigment(vec3 p) {
        return vec4(0.1, 0.45, 0.15, 1.0);
        }

        vec4 environmentColor(vec3 p, vec3 r) {
        vec4 upColor = 0.1 * roomColor(p, r);
        vec4 downColor = -r.z * 0.3 * feltPigment(p);
        float t = smoothstep(0.0, 0.05, r.z);
        return mix(downColor, upColor, t);
        }

        vec4 solidPixelShader() {
        return vec4(1.0, 1.0, 1.0, 0.2);
        }

        vec4 feltPixelShader() {
        vec2 tex = vscreenPosition.xy / vscreenPosition.w;

        vec3 p = factor * vworldPosition;
        vec3 c = factor * eyeWorldPosition.xyz;
        float width = 0.3;
        float height = 0.3;
        float d =
            1.0 * (smoothstep(1.0 - width, 1.0 + width, abs(p.x)) +
                    smoothstep(2.0 - height, 2.0 + height, abs(p.y)));
        p = vworldPosition;

        return (1.0 - texture2D(textureSampler, tex).x - d) *
            lighting(feltPigment(p), 4.0);
        }

        vec4 woodPixelShader() {
        vec3 p = factor * vworldPosition;
        return lighting(woodPigment(p), 50.0);
        }

        vec4 cushionPixelShader() {
        vec3 p = factor * vworldPosition;
        return lighting(feltPigment(p), 4.0);
        }
 
        vec4 billiardPixelShader() {
        vec3 p = factor * vworldPosition;
        return lighting(vec4(0.5, 0.5, 0.2, 1), 30.0);
        }

        vec4 ballPixelShader() {
        vec3 p = normalize(vobjectPosition.xyz);
        vec4 u = 0.5 * vec4(p.x, p.y, p.x, -p.y);
        u = clamp(u, -0.45, 0.45);
        u += vec4(0.5, 0.5, 0.5, 0.5);

        float t = clamp(5.0 * p.z, 0.0, 1.0);

        p = vworldPosition;
        vec3 l = normalize(lightWorldPosition - p); // Toward light.
        vec3 n = normalize(vnormal);                // Normal.
        vec3 v = normalize(eyeWorldPosition - p);   // Toward eye.
        vec3 r = normalize(-reflect(v, n));         // Reflection of v across n.

        vec4 pigment =
            mix(texture2D(textureSampler, u.zw),
                texture2D(textureSampler, u.xy), t);

        return 0.4 * environmentColor(p, r) +
            pigment * (0.3 * smoothstep(0.0, 1.1, dot(n, l)) +
                        0.3 * (p.z + 1.0));
        }

        vec4 shadowPlanePixelShader() {
        vec2 p = vworldPosition.xy - ballCenter;
        vec2 q = (vworldPosition.xy / lightWorldPosition.z);

        vec2 offset = (1.0 - 1.0 / (vec2(1.0, 1.0) + abs(q))) * sign(q);
        float t = mix(smoothstep(0.9, 0.0, length(p - length(p) * offset) / 2.0),
                        smoothstep(1.0, 0.0, length(p) / 10.0), 0.15);
        return shadowOn * vec4(t, t, t, t);
        }"

    [<JavaScript>]
    let FinishLoadingBitmaps(bitmaps : O3D.Bitmap[], exn) =
        let bitmap = bitmaps.[0]
        bitmap.FlipVertically()
        let width = bitmap.Width / 4
        let height = bitmap.Height / 4
        let levels = O3DJS.Texture.ComputeNumLevels(width, height)
        for i = 0 to 15 do
            g_ballTextures.[i] <- g_pack.CreateTexture2D(width, height, O3D.Texture.XRGB8, 0, false)
            g_ballTextureSamplers.[i] <- g_pack.CreateSampler(Texture = g_ballTextures.[i])
        for i = 0 to 15 do
            let u = i % 4
            let v = i / 4
            g_ballTextures.[i].DrawImage(bitmap, 0, u * width, v * height, width, height, 0, 0, 0, width, height)
            g_ballTextures.[i].GenerateMips(0, levels - 1)
        for i = 0 to 15 do
            g_ballTextureSamplerParams.[i].Value <- g_ballTextureSamplers.[i]

    [<JavaScript>]
    let InitMaterials() =
        g_materials <- new obj()
        [|"solid"; "felt"; "wood"; "cushion"; "billiard"; "ball"; "shadowPlane"|]
        |> Array.iter (fun name ->
            let effect = g_pack.CreateEffect()
            let mainString = "void main() { gl_FragColor = " + name + "PixelShader(); }"
            ignore <| effect.LoadVertexShaderFromString VertexShaderString
            ignore <| effect.LoadPixelShaderFromString (PixelShaderString + mainString)
            let material = g_pack.CreateMaterial(Effect = effect,
                                                 DrawList = g_viewInfo.PerformanceDrawList)
            (?<-) g_materials name material
            effect.CreateUniformParameters material
            let eye, target = g_cameraInfo.GetEyeAndTarget()
            SetOptionalParam(material, "factor", 2. / g_tableWidth)
            SetOptionalParam(material, "lightWorldPosition", g_light)
            SetOptionalParam(material, "eyeWorldPosition", eye))
        g_solidMaterial <- g_materials?solid
        g_solidMaterial.DrawList <- g_hudViewInfo.ZOrderedDrawList
        (As<O3D.Material> g_materials?shadowPlane).DrawList <- g_shadowPassViewInfo.ZOrderedDrawList
        g_shadowSampler <- g_pack.CreateSampler(Texture = g_shadowTexture)
        ((As<O3D.Material> g_materials?felt).GetParam "textureSampler").Value <- g_shadowSampler
        O3DJS.Io.LoadBitmaps(g_pack,
                             O3DJS.Util.GetAbsoluteURI("../assets/poolballs.png"),
                             FinishLoadingBitmaps) |> ignore

    [<JavaScript>]
    let FlatMesh(material, vertexPositions : Float3[], faceIndices) =
        let vertexInfo = O3DJS.Primitives.CreateVertexInfo()
        let positionStream = vertexInfo.AddStream(3, O3D.Stream.POSITION)
        let normalStream = vertexInfo.AddStream(3, O3D.Stream.NORMAL)
        faceIndices |> Array.fold (fun vertexCount (faceX, faceY, faceZ, faceW) ->
            let n = O3DJS.Math.Normalize(O3DJS.Math.Cross(O3DJS.Math.Sub(vertexPositions.[faceY],
                                                                         vertexPositions.[faceX]),
                                                          O3DJS.Math.Sub(vertexPositions.[faceZ],
                                                                         vertexPositions.[faceX])))
            let faceFirstIndex = vertexCount
            [|faceX; faceY; faceZ; faceW|] |> Array.iter (fun face_j ->
                let v = vertexPositions.[face_j]
                positionStream.AddElementVector v
                normalStream.AddElementVector n)
            vertexInfo.AddTriangle(faceFirstIndex, faceFirstIndex+1, faceFirstIndex+2)
            vertexInfo.AddTriangle(faceFirstIndex, faceFirstIndex+2, faceFirstIndex+3)
            vertexCount + 4) 0
        |> ignore
        vertexInfo.CreateShape(g_pack, material)

    [<JavaScript>]
    let Arc((centerX, centerY), radius, start, end', steps) =
        Array.init (steps + 1) (fun i ->
            let theta = start + float i * (end' - start) / float steps
            [|centerX + radius * Math.Cos theta; centerY + radius * Math.Sin theta|])


    [<JavaScript>]
    let Flip(a : float[][], b : float[]) =
        let myreverse (l : Float2[]) =
            let n = l.Length
            Array.init n (fun i -> if i = 0 then l.[0] else l.[n - i])
        let r =
            Array.init a.Length (fun i -> (b.[0] * a.[i].[0], b.[1] * a.[i].[1]))
        if b.[0] * b.[1] < 0. then
            myreverse r
        else
            r

    [<JavaScript>]
    let InitShadowPlane() =
        let root = g_pack.CreateTransform(Parent = g_shadowRoot)
        let plane = O3DJS.Primitives.CreatePlane(g_pack,
                                                 As<O3D.Material> g_materials?shadowPlane,
                                                 g_tableWidth,
                                                 g_tableWidth * 2., 1, 1)
        root.Translate((0., 0., -1.))
        root.RotateX(System.Math.PI / 2.)
        let transforms = Array.init 16 (fun i ->
            let transform = g_pack.CreateTransform(Parent = root)
            transform.AddShape plane
            transform)
        g_centers <- transforms |> Array.map (fun transform ->
            transform.CreateParamFloat2 "ballCenter")
        g_shadowOnParams <- transforms |> Array.map (fun transform ->
            transform.CreateParamFloat("shadowOn", Value = 1.))

    let mutable g_seriousness = 0
    let mutable g_shooting_timers = []

    [<JavaScript>]
    let ComputeShot(i, j,
                    (cx, cy, _ as cueCenter : Float3),
                    (ox, oy, _ as objectCenter : Float3),
                    (px, py as pocketCenter : Float2)) =
        let second = O3DJS.Math.Sub((px, py), (cx, cy))
        let secondDistance = O3DJS.Math.Length(second)
        let toPocket = O3DJS.Math.Div(second, secondDistance)
        let toObject = O3DJS.Math.Normalize((ox - cx, ox - cx))
        let cc = if O3DJS.Math.Dot(toObject, toPocket) > 0.8 then 0.4 else 0.
        let cut = O3DJS.Math.Mul(2. + cc, toPocket)
        let target = O3DJS.Math.Sub((ox, oy), cut)
        let first = O3DJS.Math.Sub(target, (cx, cy))
        let firstDistance = O3DJS.Math.Length(first)
        let cutAmount = 1. - O3DJS.Math.Dot(first, second) / (firstDistance * secondDistance)
        let power = 0.12 * (firstDistance + secondDistance / (1.01 * cutAmount))
        let difficulty = cutAmount * cutAmount - 0.5 / (1. + secondDistance / 2.)
        let difficulty =
            if difficulty < 1. then
                let walls =
                    [| (cx, cy), target
                       (ox, oy), (px, py) |]
                    |> Wall.ComputeNormals
                let collisions = g_physics.CollideWithWalls(walls, 1.99)
                if List.length collisions > 2 then
                    difficulty + 10.
                else difficulty
            else difficulty
        { Target = target
          Power = Math.Min(1., Math.Max(0.1, power))
          Difficulty = difficulty }

    [<JavaScript>]
    let InitTable() =
        let feltMaterial = g_materials?felt
        let woodMaterial = g_materials?wood
        let cushionMaterial = g_materials?cushion
        let billiardMaterial = g_materials?billiard
        let mutable shapes = [||]
        let root = g_pack.CreateTransform(Parent = g_tableRoot)
        let tableRoot = g_pack.CreateTransform(Parent = root)
        tableRoot.Translate(0., 0., -g_tableThickness / 2. - 1.)
        let cushionRoot = g_pack.CreateTransform(Parent = tableRoot)
        let ballRoot = g_pack.CreateTransform(Parent = root)
        let root2 = Math.Sqrt 2.
        let scaledPocketRadius = 2. * g_pocketRadius / g_tableWidth
        let scaledWoodBreadth = 2. * g_woodBreadth / g_tableWidth
        let hsrr2 = 0.5 * root2 * scaledPocketRadius
        let felt_polygonA =
            Array.append
                [| [|0.; -2.|]; [|0.; (1. + 0.5*root2) * scaledPocketRadius - 2.|] |]
                (Arc((hsrr2 - 1., hsrr2 - 2.),
                     scaledPocketRadius, 0.5*System.Math.PI, -0.25*System.Math.PI, 15))
        let felt_polygonB =
            Array.append
                [| [|-1.; (1. + 0.5*root2) * scaledPocketRadius - 2.|] |]
                (Arc((hsrr2 - 1., hsrr2 - 2.),
                     scaledPocketRadius, 0.75*System.Math.PI, 0.5*System.Math.PI, 15))
        let felt_polygonC =
            Array.concat [
                [| [|0.; (1. + 0.5*root2) * scaledPocketRadius - 2.|]; [|0.; 0.|] |]
                Arc((-1., 0.), scaledPocketRadius, 0., -0.5 * System.Math.PI, 15)
                [| [|-1.; (1. + 0.5*root2) * scaledPocketRadius - 2.|] |]
            ]
        let wood_polygon =
            Array.concat [
                [| [|-scaledWoodBreadth - 1.; -scaledWoodBreadth - 2.|]
                   [|0.; -scaledWoodBreadth - 2.|]
                   [|0.; -2.|] |]
                Arc((hsrr2 - 1., hsrr2 - 2.),
                    scaledPocketRadius, -0.25*System.Math.PI, -1.25*System.Math.PI, 15)
                Arc((-1., 0.), scaledPocketRadius, 1.5*System.Math.PI, System.Math.PI, 15)
                [| [|-scaledWoodBreadth - 1.; 0.|] |]
            ]
        let m = O3DJS.Math.Mul(g_tableWidth / 2., O3DJS.Math.Identity 2)
        let felt_polygon_A = O3DJS.Math.Mul(felt_polygonA, m)
        let felt_polygon_B = O3DJS.Math.Mul(felt_polygonB, m)
        let felt_polygon_C = O3DJS.Math.Mul(felt_polygonC, m)
        let wood_polygon = O3DJS.Math.Mul(wood_polygon, m)
        let ij = [| [|-1.; -1.|]
                    [|-1.;  1.|]
                    [| 1.; -1.|]
                    [| 1.;  1.|]|]
        let felt_polygons = ij |> Array.map (fun ij -> [|Flip(felt_polygon_A, ij)
                                                         Flip(felt_polygon_B, ij)
                                                         Flip(felt_polygon_C, ij)|])
                               |> Array.concat
        let wood_polygons = ij |> Array.map (fun ij -> Flip(wood_polygon, ij))
        let felt_shapes = felt_polygons |> Array.map (fun poly ->
            O3DJS.Primitives.CreatePrism(g_pack, feltMaterial, poly, g_tableThickness))
        shapes <- Array.append shapes felt_shapes
        let wood_shapes = wood_polygons |> Array.map (fun poly ->
            O3DJS.Primitives.CreatePrism(g_pack, woodMaterial, poly, g_tableThickness + 2. * g_woodHeight))
        shapes <- Array.append shapes wood_shapes
        let t = g_pack.CreateTransform(Parent = tableRoot)
        shapes |> Array.iter t.AddShape

        let cushionHeight = 1.1 * g_woodHeight
        let cushionUp = g_tableThickness / 2.
        let cushionProp = 0.9 * g_woodHeight
        let cushionDepth = g_tableWidth
        let cushionBreadth = g_pocketRadius
        let cushionSwoop = g_pocketRadius
        let angles = [|0.; System.Math.PI / 2.; System.Math.PI; System.Math.PI; 3. * System.Math.PI / 2.; 0.|]
        let translations = O3DJS.Math.Mul([|[|-1.; -1.; 0.|]
                                            [| 0.; -2.; 0.|]
                                            [| 1.; -1.; 0.|]
                                            [| 1.;  1.; 0.|]
                                            [| 0.;  2.; 0.|]
                                            [|-1.;  1.; 0.|]|],
                                          [|[|g_tableWidth/2.; 0.; 0.|]
                                            [|0.; g_tableWidth/2.; 0.|]
                                            [|0.; 0.; 1.|]|])
        let shortenings = O3DJS.Math.Mul(g_pocketRadius, [|[|   1.; root2|]
                                                           [|root2; root2|]
                                                           [|root2;    1.|]|])
        let billiardThickness = 0.1
        let billiardBreadth = 1.
        let billiardDepth = 0.309
        let billiardOut = -g_woodBreadth/2.
        let billiardSpacing = g_tableWidth/4.

        let billiards =
            [|-1.; 0.; 1.|] |> Array.map (fun i ->
                O3DJS.Primitives.CreatePrism(g_pack, billiardMaterial,
                                             [| (billiardOut + billiardBreadth / 2., i * billiardSpacing)
                                                (billiardOut, billiardDepth + i * billiardSpacing)
                                                (billiardOut - billiardBreadth / 2., i * billiardSpacing)
                                                (billiardOut, -billiardDepth + i * billiardSpacing) |],
                                             g_tableThickness + 2. * g_woodHeight + billiardThickness))
        for i = 0 to 5 do
            let backShortening = shortenings.[i%3].[1]
            let frontShortening = shortenings.[i%3].[0]
            let vertexPositions = [| (0., -cushionDepth / 2. + backShortening, cushionUp)
                                     (cushionBreadth, -cushionDepth / 2. + cushionSwoop + backShortening, cushionUp + cushionProp)
                                     (cushionBreadth, -cushionDepth / 2. + cushionSwoop + backShortening, cushionUp + cushionHeight)
                                     (0., -cushionDepth / 2. + backShortening, cushionUp + cushionHeight)
                                     (0., cushionDepth / 2. - frontShortening, cushionUp)
                                     (cushionBreadth, cushionDepth / 2. - cushionSwoop - frontShortening, cushionUp + cushionProp)
                                     (cushionBreadth, cushionDepth / 2. - cushionSwoop - frontShortening, cushionUp + cushionHeight)
                                     (0., cushionDepth / 2. - frontShortening, cushionUp + cushionHeight) |]
            let faceIndices = [| (0, 1, 2, 3)
                                 (7, 6, 5, 4)
                                 (1, 0, 4, 5)
                                 (2, 1, 5, 6)
                                 (3, 2, 6, 7)
                                 (0, 3, 7, 4) |]
            let cushion = FlatMesh(cushionMaterial, vertexPositions, faceIndices)
            shapes <- Array.append shapes [|cushion|]
            let t = g_pack.CreateTransform(LocalMatrix = O3DJS.Math.Mul(O3DJS.Math.Matrix4.RotationZ(angles.[i]),
                                                                        O3DJS.Math.Matrix4.Translation(translations.[i])),
                                           Parent = cushionRoot)
            t.AddShape cushion
            billiards |> Array.iter t.AddShape
        shapes <- Array.append shapes billiards
        let ball = O3DJS.Primitives.CreateSphere(g_pack, g_materials?ball, 1., 50, 70)
        shapes <- Array.append shapes [|ball|]
        for i = 0 to 15 do
            let transform = g_pack.CreateTransform(Parent = ballRoot)
            g_ballTextureSamplerParams.[i] <- transform.CreateParamSampler "textureSampler"
            g_ballTransforms.[i] <- transform
            transform.AddShape ball

    [<JavaScript>]
    let InitHud() =
        let barT1 = g_pack.CreateTransform(Parent = g_hudRoot)
        g_barScaling <- g_pack.CreateTransform(Parent = barT1)
        let barT2 = g_pack.CreateTransform(Parent = g_barScaling)
        let backT2 = g_pack.CreateTransform(Parent = barT1)
        g_barRoot <- barT1
        let plane = O3DJS.Primitives.CreatePlane(g_pack, g_solidMaterial, 1., 1., 1, 1,
                                                 ((1., 0., 0., 0.),
                                                  (0., 0., 1., 0.),
                                                  (0.,-1., 0., 0.),
                                                  (0., 0., 0., 1.)))
        let backPlane = O3DJS.Primitives.CreatePlane(g_pack, g_solidMaterial, 1., 1., 1, 1,
                                                     ((1., 0., 0., 0.),
                                                      (0., 0., 1., 0.),
                                                      (0.,-1., 0., 0.),
                                                      (0., 0., 0., 1.)))
        barT2.AddShape(plane)
        barT1.Translate((0.05, 0.05, 0.))
        barT1.Scale((0.05, 0.9, 1.))
        g_barScaling.LocalMatrix <- O3DJS.Math.Matrix4.Scaling((1., 0., 1.))
        barT2.Translate((0.5, 0.5, 0.))
        backT2.Translate((0.5, 0.5, 0.1))

    [<JavaScript>]
    let SetBarScale t =
        g_barScaling.LocalMatrix <- O3DJS.Math.Matrix4.Scaling((1., t, 1.))

    [<JavaScript>]
    let Onrender (event : O3D.RenderEvent) =
        g_clock <- g_clock + event.ElapsedTime
        g_queueClock <- g_queueClock + event.ElapsedTime
        let clock = g_queueClock
        if g_queue.Length > 0 && g_queue.[0].condition clock then
            let action = g_queue.[0].action
            g_queue <- Array.sub g_queue 1 (g_queue.Length - 1)
            action()
            g_queueClock <- 0.
        g_cameraInfo.UpdateClock()
        if g_physics.SomeBallsMoving() then
            g_physics.Step()
            g_physics.StopSlowBalls()
        elif g_rolling then
            g_rolling <- false
            let cueBall = g_physics.Balls.[0]
            if g_cameraInfo.LookingAt cueBall.Center then
                g_barRoot.Visible <- true
            if not cueBall.Active then
                g_physics.BallOn 0
                cueBall.Center <- (0., 0., 0.)
                g_physics.BoundCueBall()
        UpdateContext()

    [<JavaScript>]
    let IncreaseFactor() =
        g_physics.SpeedFactor <- Math.Min(g_physics.SpeedFactor + 0.01, 1.)
        SetBarScale(g_physics.SpeedFactor)

    [<JavaScript>]
    let StartShooting() =
        g_shooting <- true
        g_shooting_timers <- JS.SetInterval IncreaseFactor (1000/60) :: g_shooting_timers

    [<JavaScript>]
    let FinishShooting() =
        g_shooting_timers |> List.iter JS.ClearTimeout
        g_shooting_timers <- []
        if g_physics.SpeedFactor > 0. then
            let (ex, ey, _), (tx, ty, _) = g_cameraInfo.GetEyeAndTarget()
            let d = O3DJS.Math.Sub((tx, ty), (ex, ey))
                    |> O3DJS.Math.Normalize
            g_physics.ImpartSpeed(0, d)
            g_cameraInfo.BackUp()
            g_rolling <- true
            g_barRoot.Visible <- false
        g_physics.SpeedFactor <- 0.
        g_seriousness <- 0
        SetBarScale g_physics.SpeedFactor
        g_shooting <- false

    [<JavaScript>]
    let Rack(game) =
        let root3 = Math.Sqrt 3.
        let yOffset = 6. * g_tableWidth / 12.
        let cueYOffset = - g_tableWidth / 2.
        for i = 0 to 15 do
            g_physics.BallOn i
        g_physics.StopAllBalls()
        match game with
        | 8 ->
            g_physics.PlaceBall(1, (0., 0. + yOffset, 0.))
            g_physics.PlaceBall(9, (-1., root3 + yOffset, 0.))
            g_physics.PlaceBall(2, (1., root3 + yOffset, 0.))
            g_physics.PlaceBall(10, (2., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(8, (0., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(3, (-2., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(11, (-3., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(4, (-1., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(12, (1., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(5, (3., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(13, (4., 4. * root3 + yOffset, 0.))
            g_physics.PlaceBall(6, (2., 4. * root3 + yOffset, 0.))
            g_physics.PlaceBall(14, (0., 4. * root3 + yOffset, 0.))
            g_physics.PlaceBall(15, (-2., 4. * root3 + yOffset, 0.))
            g_physics.PlaceBall(7, (-4., 4. * root3 + yOffset, 0.))
            g_physics.PlaceBall(0, (0., cueYOffset, 0.))
        | 9 ->
            g_physics.PlaceBall(1, (0., 0. + yOffset, 0.))
            g_physics.PlaceBall(2, (1., root3 + yOffset, 0.))
            g_physics.PlaceBall(3, (-1., root3 + yOffset, 0.))
            g_physics.PlaceBall(9, (0., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(4, (2., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(5, (-2., 2. * root3 + yOffset, 0.))
            g_physics.PlaceBall(6, (1., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(7, (-1., 3. * root3 + yOffset, 0.))
            g_physics.PlaceBall(8, (0., 4. * root3 + yOffset, 0.))
            for i = 10 to 15 do
                g_physics.PlaceBall(i, (0., 0., -5.))
                g_physics.BallOff i
            g_physics.PlaceBall(0, (0., cueYOffset, 0.))
        | 0 ->
            for i = 1 to 15 do
                g_physics.PlaceBall(i, (0., 0., -5.))
                g_physics.BallOff i
            g_physics.PlaceBall(0, (0., 0., cueYOffset))
        | 1 ->
            for i = 1 to 15 do
                g_physics.PlaceBall(i, (0., 0., -5.))
                g_physics.BallOff i
            g_physics.PlaceBall(0, (0., cueYOffset, 0.))
            g_physics.PlaceBall(1, (-g_tableWidth/4., cueYOffset/2., 0.))
            g_physics.PlaceBall(2, (-3.*g_tableWidth/8., cueYOffset/4., 0.))
            g_physics.PlaceBall(3, (g_tableWidth/4., 0., 0.))
            g_physics.BallOn 0
            g_physics.BallOn 1
            g_physics.BallOn 2
            g_physics.BallOn 3
        | _ ->
            failwith "MatchFailure"
        g_physics.RandomOrientations()
        g_physics.PlaceBalls()
        g_cameraInfo.GoTo(Some (0., 0., 0.), Some (-System.Math.PI/2.), Some (System.Math.PI/6.), Some 140.)

    [<JavaScript>]
    let KeyUp(event : O3D.Event) =
       if event.KeyCode = 32 then
           FinishShooting()

    [<JavaScript>]
    let KeyDown(event : O3D.Event) =
        ()

    [<JavaScript>]
    let ZoomIn() =
        g_cameraInfo.targetPosition.Radius <- g_cameraInfo.targetPosition.Radius * 0.9

    [<JavaScript>]
    let ZoomOut() =
        g_cameraInfo.targetPosition.Radius <- g_cameraInfo.targetPosition.Radius / 0.9

    [<JavaScript>]
    let ScrollWheel(event : O3D.Event) =
        if event.DeltaY > 0 then
            ZoomIn()
        else
            ZoomOut()

    [<JavaScript>]
    let KeyPressed(event : O3D.Event) =
        let keyChar = (String.FromCharCode [|O3DJS.Event.GetEventKeyChar(event)|]).ToLower()
        let spotDelta = 1.
        let cueBall = g_physics.Balls.[0]
        let (x, y, _) = cueBall.Center
        match keyChar with
        | "*" ->
            Rack 8
        | "(" ->
            Rack 9
        | ")" ->
            Rack 0
        | "d" ->
            g_physics.BallOn 0
            g_physics.PlaceBall(0, (x + spotDelta, y, 0.))
            g_physics.BoundCueBall()
        | "a" ->
            g_physics.BallOn 0
            g_physics.PlaceBall(0, (x - spotDelta, y, 0.))
            g_physics.BoundCueBall()
        | "s" ->
            g_physics.BallOn 0
            g_physics.PlaceBall(0, (x, y - spotDelta, 0.))
            g_physics.BoundCueBall()
        | "w" ->
            g_physics.BallOn 0
            g_physics.PlaceBall(0, (x, y + spotDelta, 0.))
            g_physics.BoundCueBall()
        | "c" ->
            g_cameraInfo.ZoomToPoint g_physics.Balls.[0].Center
            if not g_rolling then
                g_barRoot.Visible <- true
        | "t" ->
            g_cameraInfo.GoTo(Some (0., 0., 0.), None, None, Some 100.)
        | "=" | "+" ->
            ZoomIn()
        | "-" | "_" ->
            ZoomOut()
        | " " ->
            if not (g_cameraInfo.LookingAt g_physics.Balls.[0].Center) then
                g_cameraInfo.ZoomToPoint g_physics.Balls.[0].Center
                if not g_rolling then
                    g_barRoot.Visible <- true
            else
                if g_seriousness > 1 then
                    if not (g_rolling || g_shooting) then
                        StartShooting()
                g_seriousness <- g_seriousness + 1
        | _ ->
            failwith "MatchFailure"
        UpdateContext()

    [<JavaScript>]
    let SetRenderCallback() =
        g_client.SetRenderCallback Onrender

    [<JavaScript>]
    let RegisterEventCallbacks() =
        O3DJS.Event.AddEventListener(g_o3dElement, "mousedown", StartDragging)
        O3DJS.Event.AddEventListener(g_o3dElement, "mousemove", Drag)
        O3DJS.Event.AddEventListener(g_o3dElement, "mouseup", StopDragging)
        O3DJS.Event.AddEventListener(g_o3dElement, "keypress", KeyPressed)
        O3DJS.Event.AddEventListener(g_o3dElement, "keyup", KeyUp)
        O3DJS.Event.AddEventListener(g_o3dElement, "keydown", KeyDown)
        O3DJS.Event.AddEventListener(g_o3dElement, "wheel", ScrollWheel)

    [<JavaScript>]
    let Main(clientElements) =
        InitPhysics()
        InitGlobals(clientElements)
        InitRenderGraph()
        UpdateContext()
        InitMaterials()
        InitShadowPlane()
        InitTable()
        InitHud()
        Rack(8)
        SetRenderCallback()
        RegisterEventCallbacks()

    [<JavaScript>]
    member this.CueNewShot(power) =
        let cue = g_physics.Balls.[0]
        g_queue <- Array.append [| {condition = fun clock -> clock > 1.
                                    action = fun() -> g_cameraInfo.ZoomToPoint cue.Center} |]
                                g_queue
        let objectBalls' =
            Array.sub g_physics.Balls 1 7
            |> Array.filter (fun ball -> ball.Active)
        let eight = g_physics.Balls.[8]
        let objectBalls =
            if objectBalls'.Length = 0 && eight.Active then
                [| eight |]
            else
                objectBalls'
        let (current, _) =
            objectBalls
            |> Array.fold (fun (current : Shot, i) ball ->
                let (current, _) =
                    g_physics.PocketCenters
                    |> Array.fold (fun (current : Shot, j) (pcx, pcy) ->
                        let k = g_pocketRadius
                        let pcx =
                            if pcx > 1. then pcx - k
                            elif pcx < -1. then pcx + k
                            else pcx
                        let pcy =
                            if pcy > 1. then pcy - k
                            elif pcy < -1. then pcy + k
                            else pcy
                        let shot = ComputeShot(i, j, cue.Center, ball.Center, (pcx, pcy))
                        let current =
                            if shot.Difficulty < current.Difficulty
                            then shot
                            else current
                        (current, j + 1))
                       (current, 0)
                (current, i + 1))
               (Shot.Null, 0)
        if current.Difficulty <> infinity then
            let (cx, cy, _) = cue.Center
            let theta = Math.Atan2(O3DJS.Math.Sub((cx, cy), current.Target))
            let power = defaultArg power current.Power
            g_queue <- Array.append
                [|
                    { condition = fun _ -> true
                      action = fun () -> g_cameraInfo.GoTo(None, Some theta, None, Some 0.) }
                    { condition = fun clock -> clock > 1.5
                      action = fun () -> StartShooting() }
                    { condition = fun _ -> g_physics.SpeedFactor >= power
                      action = fun () -> g_physics.SpeedFactor <- power
                                         FinishShooting() }
                    { condition = fun _ -> not (g_shooting || g_rolling)
                      action = fun () -> this.CueNewShot(None) }
                |] g_queue

    [<JavaScript>]
    member this.InitClient() =
        O3DJS.Webgl.MakeClients(Main)
