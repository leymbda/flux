[<AutoOpen>]
module ArrayExtensions

module Array =
    /// Builds a new array that contains the elements of the second array followed by the elements of the first array.
    let prepend array2 array1 =
        Array.append array1 array2
