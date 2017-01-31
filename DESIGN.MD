Perpetuum in the IdHub Key Manager
==================================

<img alt="Perpetuum Mobile" src="image/240px-PerpetuumMobile.gif" style="float: right;"/>

>   *Perpetuum is a Petri-net scheduler generator. This model is very nice when
>   dealing with asynchronous or event-based activity. This is especially the
>   case for the Key Manager in IdentityHub, which is forced to deal with a lot
>   of tool or protocol specific details, making it ideal to generate structures
>   automatically — and flawlessly.*

Our Petri nets are:

-   Coloured Petri nets; colour represents the identity passing through key
    management.

-   They have inhibitor arcs.

-   They only grab one token per arc, arcs never have head and tail in common.

**Simpel schema​**

-   Onze transities tellen maar voor 1 token consumptie

-   Hierdoor hoeven we alleen te rapporteren als we 0—\>1 gaan en 1—\>0

-   Inhibiting arcs zullen net andersom rapporteren, maar staan geen tokens af

-   Iets anders (dus 2) komt niet voor, maar dat doet niet ter zaken in dit
    geval

-   Algemeen is trouwens dat je rapporteert wanneer je de threshold van de pijl
    doorkruist

-   De ontvangende transitie hoeft alleen maar bij te houden hoeveel places op
    groen staan

-   De eerste transitie die dat wil mag het proberen te vuren

-   Dit verlangt geen intelligentie bij de transitie; we kunnen omlaag tellen en
    0 herkennen

-   Een place verwijst dus naar transities, en zonodig hun
    tokenconsumptiemultiple

-   Bij binnenkomst van een token wordt dat aan transities aangeboden

-   De transitie kan even bezig zijn met proberen; zolang wordt het aanbod
    gelockt

-   Andere transities zien het token terugtrekken en kunnen hierdoor niet gaan
    vuren

-   Als de transitie doorgang vindt, dan is het token terecht gelockt /
    teruggetrokken

-   Als de transitie weigert, ga dan door met aanbod aan volgende places, tot
    rond

-   Slimmer is nog: eerste sweep kijken wie er vuren kan, anders 2e sweep
    unlocked aanbod

-   Fair is als de rondjes op een verschuivende manier plaatsvinden

-   Ook na terugtrekken van een transitie hele cycle aflopen? Risico op
    livelock...

-   Places beginnen met een lock op alle inputs (behalve de aanstichter dan)

-   Erg fijn om dit single-threaded te doen!!!

**Vertaler**

-   Een place kent al zijn opvolgers, om een token in weg te zetten

-   Een place kent zijn voorgangers niet

-   Een transitie kent al zijn opvolgers, om tokens aan te kunnen aanbieden

-   Een transitie kent al zijn voorgangers, om locks op tokens te kunnen nemen

-   Uiteindelijk zijn er dus statische tabellen met getallen nodig als
    verwijzingen

-   De tellers per place (alles en unlocked apart) staan ergens in een data
    tabel (marking)

-   De counters per transitie staan ergens in een data tabel (countdown)

-   Input tokens per transitie == lengte van voorganger-places voor die
    transitie

-   Er komt dus een stapel statische tabllen uit, verwijzingen naar dynamische

-   Er is maar heel weinig code nodig; enkel de procedure voor transitierekening

-   Slimmigheden gaan over de afmetingen van integers denk ik

-   Daarnaast callbacks bij transities, en afhandeling van de uitkomsten

-   Bovendien openstaan voor system events, zoals network traffic

-   Misschien niet slim de transitierekening af te maken als er I/O is? (vanwege
    concurrency)

-   Echter, alle transities starten asynchroon op, dus het zal wel meevallen

-   En bovendien rekenen we echt met delta’s, dus er wordt geen werk verzet
    tenzij zinvol

-   En al dat werk levert als het goed is ook concurrency op

-   Grote kans dat we place counters in een uint8_t kwijt kunnen (of zelfs in 4
    bits of 1 bit)

-   Grote kans dat we array indices in een uint16_t kwijt kunnen (of zelfs
    uint8_t)

    -   Dit vervangt dan pointers naar andere objecten op een zuiniger manier

**Transacties en Journaling**

-   We slaan transactiebevestigingen op voor we de uitgaande transities laten
    vuren

-   Let op dat we alle acties idempotent willen programmeren

-   Er is dus geen probleem bij herhalen van het gedane

-   Reeds bestaande resultaten stilzwijgend overnemen / negeren / vervangen

**Processes and Threads**

-   Single threading is een life saver — en een CPU saver

-   Multiple processes kunnen prima — verdeel gewoon de kleuren over processen

-   Dit heeft natuurlijk gevolgen voor file descriptor en socket sharing

**Monitoring**

-   Het is eenvoudig om van tokens aan te geven hoe lang ze al “vast” zitten in
    een place

-   Hiervoor de huidige tijd in een token/kleur opnemen

-   Ook kan de success rate van een transitie worden bijgehouden

-   Via SNMP zijn die dingen eenvoudig uit te lezen

-   Let op dat hierbij alle kleuren opgeteld moeten worden

-   Let op dat dit ingewikkeld wordt bij multiple processes

 
