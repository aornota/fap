module Aornota.Fap.Playlists.Temp

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.Model

let testState =
    let trackData folder name =
        { Id = TrackId.Create()
          Folder = folder
          Name = name
          Duration = None }

    let items (head: TrackData, tail: TrackData list) =
        NonEmptyList<Item>.Create (Track head, tail |> List.map (fun trackData -> Track trackData))

    let playlist1 =
        let folder = @"D:\AUDIO\_MIXES\_wip-b\candidates"

        let track1 =
            trackData folder "000 ^ félicia atkinson (with sylvain chauveau) - aberdeen.wav"

        let track2 =
            trackData folder "000 ^ félicia atkinson (with sylvain chauveau) - dans la lumière.wav"

        let track3 = trackData folder "000 ^ liran donin & idris rahman - thar.wav"
        let track4 = trackData folder "016 ^ christine ott - comma.wav"
        let track5 = trackData folder "021 ^ arve henriksen - lament.wav"
        let track6 = trackData folder "036 ^ stefano pilia - codexiii (+).wav"
        let track7 = trackData folder "050 ^ galia bisengalieva - aralkum.wav"
        let track8 = trackData folder "051 ^ ian william craig - stories.wav"

        let track9 =
            trackData folder "ERROR -> 051-056 ^ jean-philippe rameau (natacha kudritskaya) - les soupirs.wav"

        let track10 = trackData folder "056 ^ midori takada - kannon-daiji.wav"

        { Id = PlaylistId.Create()
          Name = Some "wip (mellow)"
          ItemsState =
            Items(
                items (track1, [ track2; track3; track4; track5; track6; track7; track8; track9; track10 ]),
                track3.Id,
                None
            ) }

    let playlist2 =
        let folder = @"D:\AUDIO\_MIXES\- now we are 03 -"

        let track1 =
            trackData folder "01. msylma & ismael - بداية باليمين (enter stage right).wav"

        let track2 = trackData folder "02. kelman duran - miss ii.wav"
        let track3 = trackData folder "03. dawuna - foreshadowing.wav"
        let track4 = trackData folder "04. erika de casier - friendly.wav"
        let track5 = trackData folder "05. ill considered - dervish.wav"
        let track6 = trackData folder "06. kelman duran - track iv.wav"
        let track7 = trackData folder "07. tilliander - respect existence.wav"
        let track8 = trackData folder "08. newworldaquarium - the dead bears.wav"
        let track9 = trackData folder "09. dj python feat. la warman - admsdp.wav"

        let track10 =
            trackData folder "10. le petit (donato dozzy & stefano ghittoni) - sukia.wav"

        let track11 = trackData folder "11. dj python - juntos.wav"
        let track12 = trackData folder "12. kelman duran - pompis.wav"
        let track13 = trackData folder "13. palmistry - memory taffeta.wav"
        let track14 = trackData folder "14. palmistry - dream ii.wav"
        let track15 = trackData folder "15. sangre nueva - hurt.wav"
        let track16 = trackData folder "16. msylma - li-kul-i murad-in hijaa.wav"

        { Id = PlaylistId.Create()
          Name = Some "now we are 03"
          ItemsState =
            Items(
                items (
                    track1,
                    [ track2
                      track3
                      track4
                      track5
                      track6
                      track7
                      track8
                      track9
                      track10
                      track11
                      track12
                      track13
                      track14
                      track15
                      track16 ]
                ),
                track7.Id,
                None
            ) }

    let playlist3 =
        let folder = @"D:\AUDIO\_MIXES\- sss0018 (for nick & olivia) -"

        let track1 = trackData folder "01. donato dozzy & tin man - test 3.wav"

        let track2 = trackData folder "02. plastikman - psyk.wav"

        let track3 =
            trackData folder "03. shackleton feat. vengeance tenfold - death is not final.wav"

        let track4 =
            trackData folder "04. dj sprinkles - masturjakor (bonus beats edit).wav"

        let track5 = trackData folder "05. mlz - crossed swords (alice loop).wav"

        let track6 =
            trackData folder "06. paranoid london feat. mutado pintado - eating glue (album edit).wav"

        let track7 = trackData folder "07. locked groove - oscillate.wav"
        let track8 = trackData folder "08. horseshoe - bassfunk.wav"
        let track9 = trackData folder "09. markus suckut - untitled #1.wav"

        let track10 = trackData folder "10. blackdown - this journey vip.wav"

        let track11 = trackData folder "11. overmono - so u kno.wav"
        let track12 = trackData folder "12. jack junior - play it again.wav"
        let track13 = trackData folder "13. neil landstrumm - tension in new york.wav"
        let track14 = trackData folder "14. cab drivers - steam.wav"
        let track15 = trackData folder "15. system 01 - drugs work.wav"
        let track16 = trackData folder "16. d.i.e. - no future in detroit.wav"

        { Id = PlaylistId.Create()
          Name = Some "sss0018 (for nick & olivia)"
          ItemsState =
            Items(
                items (
                    track1,
                    [ track2
                      track3
                      track4
                      track5
                      track6
                      track7
                      track8
                      track9
                      track10
                      track11
                      track12
                      track13
                      track14
                      track15
                      track16 ]
                ),
                track8.Id,
                None
            ) }

    let playlist4 =
        { Id = PlaylistId.Create()
          Name = None
          ItemsState = NoItems }

    Playlists(NonEmptyList<Playlist>.Create (playlist1, [ playlist2; playlist3; playlist4 ]))
