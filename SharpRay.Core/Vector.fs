namespace SharpRay.Core

type Vector =
    struct
        val X: float
        val Y: float
        val Z: float

        new(x, y, z) = { X=x; Y=y; Z=z }

        member v.MagnitudeSquared = v.X*v.X + v.Y*v.Y + v.Z*v.Z
        member v.Magnitude        = sqrt v.MagnitudeSquared

        static member (~-) (v: Vector) = Vector(-v.X, -v.Y, -v.Z)
        static member (*) (s: float, v: Vector) = Vector(s*v.X, s*v.Y, s*v.Z)
        static member (*) (v: Vector, s: float) = Vector(s*v.X, s*v.Y, s*v.Z)
        static member (/) (v: Vector, s: float) = v * (1.0/s)
        static member (+) (v: Vector, u: Vector) = Vector(v.X+u.X, v.Y+u.Y, v.Z+u.Z)
        static member (-) (v: Vector, u: Vector) = Vector(v.X-u.X, v.Y-u.Y, v.Z-u.Z)
    end

module Vec =
    let dot   (v: Vector) (u: Vector) = v.X*u.X + v.Y*u.Y + v.Z*u.Z
    let cross (v: Vector) (u: Vector) = Vector(v.Y*u.Z - v.Z*u.Y, v.Z*u.X - v.X*u.Z, v.X*u.Y - v.Y*u.X)
    let normalize (v: Vector) = v / v.Magnitude
