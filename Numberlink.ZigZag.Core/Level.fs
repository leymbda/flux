namespace Numberlink.ZigZag.Core

open System

type Level = {
    Id: Guid
    Template: Template
    Links: Guid list list
}

module Level =
    /// Create an existing level with predefined links.
    let create levelId template links =
        { Id = levelId; Template = template; Links = links }

    /// Generate a new level based on the given template.
    let generate random levelId (template: Template) =
        let links = Template.generate random template

        { Id = levelId; Template = template; Links = links }
