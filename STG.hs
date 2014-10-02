{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
module STG where
import Language.LBNF


bnfc [lbnf|

token UIdent (upper (letter | digit | '_')*) ;

rules Constructor ::= UIdent ;

token LIdent (lower (letter | digit | '_')*) ;

IntLiteral. Lit ::= Integer ;
V.        Var ::= LIdent ;

Var.     Atom ::= Var     ;
Literal. Atom ::= Lit   ;

separator Atom ","   ;
rules Atoms ::= "{" [Atom] "}" ;

separator Var ","     ;
rules Vars ::= "{" [Var] "}" ;

rules Prim ::= "+" | "-" ;

Binding. Binding ::= Var "=" LambdaForm ;

rules LambdaForm ::= Vars "\"" Pi Vars "->" Expr ;
rules Pi ::= "u" | "n" ;

separator Binding ";"  ;
rules Bindings ::= [Binding] ;

rules Expr ::= "let" Bindings "in" Expr
           | "letrec" Bindings "in" Expr
           | "case" Expr "of" Var "{" Alts "}"
           | Var Atoms
           | Constructor Atoms
           | Prim Atoms
           | Lit 
           ;

rules Alts ::= Aalts | Palts ;
rules Aalts ::= [Aalt] ";" Default ;
rules Palts ::= [Palt] ";" Default ;
        
separator Aalt ";" ;        
rules Aalt ::= Constructor Vars "->" Expr ;

separator Palt ";" ;
rules Palt ::= Lit "->" Expr ;
rules Default ::= "_DEFAULT_" "->" Expr ;


|]
