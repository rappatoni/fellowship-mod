%% \usemodule[JLogic/smglol/Commercial-Law]{mod/accounting?annual-result}
%% \usemodule[JLogic/smglol/Commercial-Law]{mod/accounting?distribution}
#pred satzung :: '\usemodule[JLogic/smglol/Corporate-Law]{mod/corporation?Satzung}'.
#pred gesellschaft :: '\usemodule[JLogic/smglol/Corporate-Law]{mod?Gesellschaft}'.
#pred gesellschaft_struct :: '\usestructure{Gesellschaft}'.
#pred zustimmung :: '\usemodule[JLogic/smglol/Civil-Law]{mod?consent}'.
#pred verbot :: '\usemodule[JLogic/smglol/Legal-Primitives]{mod?prohibition}'.
#pred gmbh :: '\usemodule[JLogic/smglol/Corporate-Law]{mod/corporation?GmbH}'.
#pred gmbh_struct :: '\usestructure{GmbH}'.
#pred gesellschafterbeschluss :: '\usemodule[JLogic/smglol/Corporate-Law]{mod?Gesellschafterbeschluss}'.
#pred gewinnverteilung :: '\usemodule[JLogic/smglol/Commercial-Law]{mod/accounting?allocation-of-earnings}'.
#pred ergebnisverwendung :: '\usemodule[JLogic/smglol/Commercial-Law]{mod/accounting?allocation-of-earnings}'.
#pred zulaessig(CompanyID) :: 'Die Ergebnisverwendungsklausel der Firma @(CompanyID) ist zulässig'.
#pred oeffnungzulaessig(CompanyID) :: 'Die Öffnungsklausel der \sr{allocation of earnings}{Ergebnisverwendung} der Firma @(CompanyID) ist zulässig'.
#pred einstimmig(CompanyID) :: 'Der \sn{Gesellschafterbeschluss} bei der Öffnung der \sr{allocation of earnings}{Ergebnisverwendung} der Firma @(CompanyID) muss einstimmig erfolgen'.
#pred existsBlackshare(CompanyID) :: 'Es existieren keine \sr{Geschaeftsanteil}{Geschäftsanteile} der Firma @(CompanyID), welche kein \sr{Stimmberechtigung}{Stimmrecht} und keinen Liquidationsanteil haben und deren Ausschluss nicht durch den Satzungstext \sr{prohibition}{verboten} ist.'.
#pred zustimmungBenachteiligt(CompanyID) :: 'Der \sn{Gesellschafterbeschluss} bei der Öffnung der \sr{allocation of earnings}{Ergebnisverwendung} der Firma @(CompanyID) muss unter \sr{consent?consent}{Zustimmung} der benachteiligten \sn{Gesellschafter}:innen erfolgen'.
#pred nichtVorhanden(CompanyID) :: 'Die Öffnungsklausel ist in der \sn{Satzung} der Firma @(CompanyID) nicht vorhanden'.
oeffnungzulaessig(G) :- einstimmig(G), not existsBlackshare(G).
oeffnungzulaessig(G) :- zustimmungBenachteiligt(G).
oeffnungzulaessig(G) :- nichtVorhanden(G).

#pred verteilungunzulaessig(CompanyID) :: 'Die \sr{distribution}{Verteilung} des \sr{annual result}{Jahresergebnisses} der Firma @(CompanyID) ist \sr{prohibition}{unzulässig}'.
#pred shares(CompanyId, Sh) :: '@(Sh) ist ein \sr{Geschaeftsanteil}{Geschäftsanteil}(-seigner) der Firma @(CompanyID)'.
#pred gewinnausschluss(CompanyId, Sh) :: '@(Sh) ist ein \sr{Geschaeftsanteil}{Geschäftsanteil}(-seigner) der Firma @(CompanyID), welcher vom \sr{annual result}{Jahresergebniss} ausgeschlossen ist'.
#pred keinLiquidationsanteil(CompanyId, Sh) :: '@(Sh) ist ein \sr{Geschaeftsanteil}{Geschäftsanteil}(-seigner) der Firma @(CompanyID) ohne Anteil am Liquiditionserlös'.
#pred keinStimmrecht(CompanyId, Sh) :: '@(Sh) ist ein \sr{Geschaeftsanteil}{Geschäftsanteil}(-seigner) der Firma @(CompanyID) ohne Stimmanteil'.
verteilungunzulaessig(G) :- shares(G, Share), gewinnausschluss(G, Share), keinLiquidationsanteil(G, Share), keinStimmrecht(G, Share).

zulaessig(G) :- not verteilungunzulaessig(G), oeffnungzulaessig(G).

company(beispiel).
einstimmig(beispiel).
satzung.
gesellschaft.
gesellschaft_struct.
zustimmung.
verbot.
gmbh.
gmbh_struct.
gesellschafterbeschluss.
gewinnverteilung.
ergebnisverwendung.

uris :- satzung, gesellschaft,  gesellschaft_struct, zustimmung, verbot, gmbh, gmbh_struct, gesellschafterbeschluss, gewinnverteilung, ergebnisverwendung.
:- not uris.

?- oeffnungzulaessig(beispiel).
             