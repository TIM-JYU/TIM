# Avaa docker

Suorita seuraava komento `Ephemeral` hakemistossa: 

    docker run -i -t -v $PWD:/Ephemeral ephemeral

Nyt olet docker kuvassa, jossa on melkein toimiva build-ympäristö
ja sorsat hakemistossa /Ephemeral

# Korjaa build-ympäristö

(Samuel: zip-archive ei vaikuttanut olevan asennettuna, joten 
         tarvi asentaa käsityönä komennolla "cabal install zip-archive")

Ihan kaikki ei kuitenkaan asentunut tähän docker kuvaan, joten
joudut asentamaan loput. Tähän menee kuitenkin vain hetki:


    cd /Ephemeral
    cabal install --only-dependencies

# Käännä

Kääntäminen onnistuu nyt komennolla

    cabal configure
    cabal build

minkä jälkeen sinulla on binääri `Ephemeral` 
hakemistossa `dist/build/Ephemeral/`. Tämän
jälkeen voit poistua dockerista (`exit`) ja
hävittää turhan kontin (`docker rm <hash joka oli kontissa
promptissa>`.

# Aja

`Ephemeral` käynnistyy nyt komennolla `dist/build/Ephemeral/Ephemeral -p
 8000`. Voit myös valita jonkun muun portin kuin 8000.

# Kokeile

Voit kokeilla `Ephemeralia` vaikka näin:

   # Ladataan cacheen testidokkari
   curl --data-binary @lecture.markdown localhost:8000/load/testidokkari

   # Haetaan tekstidokkarista kappale
   curl localhost:8000/testidokkari/1

   # Laitetaan testidokkariin uusi kappale
   curl --data-binary "kissa istuu" localhost:8000/testidokkari/1

Nykyisellään `Ephemeral` määrittelee reitit:

* POST /load/:docID                => Lataa cacheen kokonaisen dokumentin
* POST /new/:docID/:idx            => Lisää blokin dokumenttiin
* PUT  /:docID/:idx                => Korvaa yksittäisen blokin
* GET  /:docID/:idx                => Hakee yksittäisen blokin Markdown-muodossa
* GET  /:docID/:idx/html           => Hakee yksittäisen blokin HTML:nä
* GET  /:docID                     => Hakee koko dokumentin Markdown-muodossa
* GET  /html/:docID                => Hakee koko dokumentin HTML:nä
* GET  /json-html/:docID           => Hakee koko dokumentin blokkeina HTML:nä
* GET  /diff/:docID/:docID         => Hakee diffin (JSON)
* GET  /diff3/:docID/:docID/:docID => Hakee diff3:n (JSON?)


# Tulevaisuudessa..

Nyt meni suurin osa ajasta siihen, että kokosin toimivan buildi
ympäristön. Näinollen järjelliset virheilmoitukset, diffit ja merget
jäävät tulevaisuuteen.
