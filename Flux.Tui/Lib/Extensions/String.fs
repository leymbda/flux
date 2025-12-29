[<AutoOpen>]
module StringExtensions

open System

module String =
    /// Concatenates the string representations of an array of objects, using the specified separator between each
    /// member.
    let join (separator: string) (values: string seq) : string =
        String.Join(separator, values)
