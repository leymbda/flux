[<AutoOpen>]
module Array2DExtensions

module Array2D =
    /// Creates an empty 2D array.
    let empty<'T> =
        Array2D.zeroCreate<'T> 0 0

    /// Zips two 2D arrays of the same dimensions into a single 2D array of tuples.
    let zip (a: 'T1 [,]) (b: 'T2 [,]) =
        let height = Array2D.length1 a
        let width = Array2D.length2 a

        Array2D.init height width (fun i j -> (a.[i, j], b.[i, j]))

    /// Maps each row of a 2D array using the provided function and returns a 1D array of the results.
    let maprow (f: 'T1[] -> 'T2) (a: 'T1 [,]) =
        let height = Array2D.length1 a
        let width = Array2D.length2 a

        Array.init height (fun i ->
            Array.init width (fun j -> a.[i, j]) |> f
        )

    /// Maps each column of a 2D array using the provided function and returns a 1D array of the results.
    let mapcol (f: 'T1[] -> 'T2) (a: 'T1 [,]) =
        let height = Array2D.length1 a
        let width = Array2D.length2 a

        Array.init width (fun j ->
            Array.init height (fun i -> a.[i, j]) |> f
        )

    /// Folds over all elements in a 2D array using the provided function and initial state.
    let fold (f: 'State -> 'T -> 'State) (state: 'State) (a: 'T [,]) =
        let height = Array2D.length1 a
        let width = Array2D.length2 a

        let rec loop i j acc =
            match i, j with
            | i, _ when i >= height -> acc
            | i, j when j >= width -> loop (i + 1) 0 acc
            | i, j -> loop i (j + 1) (f acc a.[i, j])

        loop 0 0 state

    /// Converts a 2D array into a single continuous array.
    let toArray (a: 'T [,]) =
        let height = Array2D.length1 a
        let width = Array2D.length2 a

        Array.init (height * width) (fun i -> a.[i / width, i % width])
