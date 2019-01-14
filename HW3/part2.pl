% testing -test is night is crawler ! wadaaaa celikarcelik
% ugurkan ates 151044012 PL 341  hw2/part2.pl

flight(edirne,erzurum,5).
flight(erzurum,edirne,5).
flight(erzurum,antalya,2).
flight(antalya,erzurum,2).
flight(antalya,izmir,1).
flight(izmir,antalya,1).
flight(istanbul,izmir,3).
flight(izmir,istanbul,3).
flight(antalya,diyarbakir,5).
flight(diyarbakir,antalya,5).
flight(istanbul,trabzon,3).
flight(trabzon,trabzon,3).
flight(ankara,trabzon,6).
flight(trabzon,ankara,6).
flight(izmir,ankara,6).
flight(ankara,izmir,6).
flight(ankara,istanbul,2).
flight(istanbul,ankara,2).
flight(ankara,diyarbakir,8).
flight(diyarbakir,ankara,8).
flight(ankara,kars,3).
flight(kars,ankara,3).
flight(kars,gaziantep,3).
flight(gaziantep,kars,3).

/*Rules:*/


route(CITY,CITY2,X) :- flight(CITY,CITY2,X). 
% direct connections if exist return C

route(CITY , CITY2 , X) :- cost(CITY , CITY2 , X , []). 
% if no direct , use list to hold for vertizes

cost(CITY , CITY2 , X , _) :- flight(CITY , CITY2 , X).
% use anonymous - underscore variable , Each time it could be new so better to keep this way.
% source = http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse5


cost(CITY , CITY2 , X , LIST) :- \+ member(CITY , LIST),flight(CITY , TEMP , TEMP2), 					
							cost(TEMP , CITY2 , TEMP3 , [CITY|LIST]),CITY\=CITY2,X is TEMP2 + TEMP3.	
% \+ operator is  not provable operator. if success means 	
% member function = member(?element,?list)  ->  TRUE IF ELEMENT is member of list
%http://www.swi-prolog.org/pldoc/man?predicate=member/2
% CITY | LIST  = list head elementi al diyor her seferinde. azaltiyor listeyi bu sekilde.