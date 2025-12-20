namespace Numberlink.ZigZag.Core

open System

type Level = {
    Id: Guid
    Template: Template
    Links: Guid list list
}
