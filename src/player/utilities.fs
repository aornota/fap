module Aornota.Fap.Player.Utilities

open LibVLCSharp.Shared

let getMediaFromlocal path =
    use libvlc = new LibVLC()
    new Media(libvlc, path, FromType.FromPath)

let getEmptyPlayer =
    use libvlc = new LibVLC()
    new MediaPlayer(libvlc)
