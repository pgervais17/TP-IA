%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-  
   initial_state(S0),
   final_state(F),
   heuristique2(S0,F,H0),
   G0 is 0,
   F0 is (H0+G0),
   empty(Pf),
   empty(Pu),
   empty(Q),
   insert([[F0, H0, G0], S0], Pf, Pf1),  
   insert([S0, [F0, H0, G0],nil,nil], Pu, Pu1),
   aetoile(Pf1, Pu1, Q).

%*******************************************************************************
% Au total, il faut réaliser les opérations suivantes sur les ensembles P et Q :
%- recherche et suppression de l‘état U qui minimise fU)
%- pour chaque successeur d’un état U qui vient d’être développé, test d'appartenance à P et test d'appartenance à Q
%- insertion dans P d'une nouvelle situation U
%- mise à jour des valeurs f(S) g(S) père(S) d’un etat S de P et reclassement

aetoile(nil, nil,_) :-
   print("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ATTEIGNABLE!").
aetoile(Pf,Pu,Q):- 
   suppress_min([Val,U],Pf,Pf1), 
   suppress([U,Val,Pere,Action], Pu, Pu1),
   print(Q),
   (final_state(U) -> 
      affiche_solution(Pere,Q),
      nl,
      write(Action),
      nl,
      write_state(U)
   ;
   Val=[_,_,Gu],
   expand(Gu, U, S),
   loop_successors(S, Pf1, Pu1, Q, Pfs, Pus),
   insert([U,Val, Pere, Action], Q, Qs),
   aetoile(Pfs,Pus,Qs)
   ).

affiche_solution(P,_):- 
   initial_state(P),
   write_state(P),nl. 	
affiche_solution(P,Q) :- 
   belongs([P,_,GP,AP],Q),
   affiche_solution(GP,Q),  
   nl,
   write(AP),
   nl,
   write_state(P).




/* déterminer tous les nœuds contenant un état successeur
 S de la situation U et calculer leur évaluation [Fs, Hs, Gs]*/

expand(Gu,U,List):-
   final_state(F),
   findall([S,[Fs, Hs, Gs],U, Action], 
           ( rule(Action,K,U,S), 
             heuristique2(S,F,Hs),
             Gs is Gu + K,
             Fs is Hs+Gs
           ),
           List).


loop_successors([], Pf, Pu, _, Pf, Pu).
loop_successors([First|Rest], Pf1, Pu1, Q, Pf3, Pu3) :- 
   traiter_succ(First, Pf1, Pu1, Q, Pf2, Pu2),
   loop_successors(Rest, Pf2, Pu2, Q, Pf3, Pu3).   

traiter_succ(First, Pf1, Pu1, Q, Pf1, Pu1):- 
   First = [U,_,_,_],
   belongs([U,_,_,_],Q),
   !.
traiter_succ(First, Pf1, Pu1, _, Pf3, Pu3):- 
   First = [U,Val, _,_],
   suppress([U,Val1,_,_], Pu1, Pu2),   % il existe deja un chemin arrivant en U
   suppress([Val1, U], Pf1, Pf2),
   !,
   (Val @< Val1 ->
      Pu3 = Pu2,
      Pf3 = Pf2,
      insert([U,Val,_,_],Pu2,Pu3),
      insert([Val,U], Pf2, Pf3)
   ;
      Pf3 = Pf1,
      Pu3 = Pu1  
   ).

traiter_succ(First, Pf1, Pu1, _, Pf2, Pu2):- 
   insert(First,Pu1,Pu2), 
   First = [U,Val, _,_],
   insert([Val,U], Pf1,Pf2).


 /*print(Pf1),
         print(---),
         nl,
         print(Pu1),
         print($$$),
         nl,*/
/*S0 = [[a, b,  c],
         [vide,h, d],
         [g, f,  e]],*/

%TESTS UNITAIRES

t_avl(Q):- Q=avl(avl(nil,[[[b,h,c],[a,f,d],[g,vide,e]],[5,5,0],nil,nil],nil,0),[[[b,h,c],[a,vide,d],[g,f,e]],[5,4,1],[[b,h,c],[a,f,d],[g,vide,e]],up],avl(nil,[[[b,vide,c],[a,h,d],[g,f,e]],[5,3,2],[[b,h,c],[a,vide,d],[g,f,e]],up],avl(nil,[[[vide,b,c],[a,h,d],[g,f,e]],[5,2,3],[[b,vide,c],[a,h,d],[g,f,e]],left],nil,0),1),2).
%"Le pere : --"[[vide,b,c],[a,h,d],[g,f,e]]

test_affiche_solution1(P):- initial_state(P),
%Q=avl(nil,[[[b,h,c],[a,f,d],[g,vide,e]],[5,5,0],nil,nil],nil,0),
   affiche_solution(P,_).

test_affiche_solution(P,Q) :- P=[[vide,b,c],[a,h,d],[g,f,e]],
   Q=avl(avl(avl(nil,[[[a,b,c],[vide,h,d],[g,f,e]],[5,1,4],[[vide,b,c],[a,h,d],[g,f,e]],down],nil,0),
   [[[b,h,c],[a,f,d],[g,vide,e]],[5,5,0],nil,nil],nil,1),[[[b,h,c],[a,vide,d],[g,f,e]],[5,4,1],[[b,h,c],[a,f,d],[g,vide,e]],up],
   avl(nil,[[[b,vide,c],[a,h,d],[g,f,e]],[5,3,2],[[b,h,c],[a,vide,d],[g,f,e]],up],
   avl(nil,[[[vide,b,c],[a,h,d],[g,f,e]],[5,2,3],[[b,vide,c],[a,h,d],[g,f,e]],left],nil,0),1),2),
   affiche_solution(P,Q). 

test_expand(S):- initial_state(S0),expand(0,S0,S).

test_expand2(S):- initial_state(S0),expand(5,S0,S).

%Pour l'état final
test_expand3(S):- final_state(F), expand(0,F,S).


test_loop_successors(Pf,Pu,Q):- Pf=[[4,4,0],[[b,h,c],[a,f,d],[g,vide,e]]], Pu=[[[b,h,c],[a,f,d],[g,vide,e]], [4,4,0], nil, nil], 
 loop_successors([],Pf,Pu,Q,Pf,Pu).
test_loop_successors2(S,Pf1,Pu1):- Pf=[[4,4,0],[[b,h,c],[a,f,d],[g,vide,e]]], Pu=[[[b,h,c],[a,f,d],[g,vide,e]], [4,4,0], nil, nil], 
   t_avl(Q), loop_successors(S,Pf,Pu,Q,Pf1,Pu1).
   %Q=avl(nil,[[[vide,b,c],[a,h,d],[g,f,e]],[5,2,3],[[b,vide,c],[a,h,d],[g,f,e]],left],nil,0), loop_successors(S,Pf,Pu,Q,Pf,Pu).
%loop_successors([], Pf, Pu, _, Pf, Pu).
%loop_successors([First|Rest], Pf1, Pu1, Q, Pf3, Pu3)
test_aetoile(S):-
   final_state(F),
   heuristique2(S,F,H0),
   G0 is 0,
   F0 is (H0+G0),
   empty(Pf),
   empty(Pu),
   empty(Q),
   insert([[F0, H0, G0], S], Pf, Pf1),  
   insert([S, [F0, H0, G0],nil,nil], Pu, Pu1),
   aetoile(Pf1, Pu1, Q).
