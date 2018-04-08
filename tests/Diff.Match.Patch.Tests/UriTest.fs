module DiffMatchPatch.Tests.UriTest

open System
open System.Web
open System.Text
open Persimmon
open UseTestNameByReflection
open DiffMatchPatch

let ``Uri.Encode should same HttpUtility.UrlEncode`` = test {
  let uri = "!()_-*.aA0 ?#$%&|@\\/[]{}<>+=^~\"'`;:,あ\u0306"
  let expected = HttpUtility.UrlEncode(uri, UTF8Encoding())
  do! assertEquals expected (Uri.Encode(uri))
}

let ``Uri.Decode should same HttpUtility.UrlDecode`` = test {
  let uri = "!()_-*.aA0+%3f%23%24%25%26%7c%40%5c%2f%5b%5d%7b%7d%3c%3e%2b%3d%5e%7e%22%27%60%3b%3a%2c%e3%81%82%cc%86"
  let expected = HttpUtility.UrlDecode(uri, UTF8Encoding(false, true))
  do! assertEquals expected (Uri.Decode(uri))
}
