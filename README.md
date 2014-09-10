Scrabble-ladder
---------------

Given a word list such as [SOWPODS](http://www.freescrabbledictionary.com/sowpods.txt), create a ladder of stacked four letter words, where every vertical string of letters is also a word. Inspired by a [picture on 9gag](http://9gag.com/gag/axNO08M).

I wrote this in order to learn Haskell.

It uses only the non-blank tiles from a single scrabble set. The algorithm starts at the bottom of the ladder, adding a rung at a time until it can't find a vertical word.

As is, it finds the last such ladder where order is taken to be alphabetical, from the bottom of the ladder:

```
	DE                     FADE                     FADE                     FADE   
	A                     HAHA                     HAHA                     HAHA    
	                     DADA                     DADA                     DADA     
	                    CEDE                     CEDE                     CEDE      
	                   BABE                     BABE                     BABE       
	                  FICE                     FICE                     FICE        
	                 JAGA                     JAGA                     JAGA         
	                LING                     LING                     LING          
	               NINE                     NINE                     NINE           
	              PINK                     PINK                     PINK            
	             GIRN                     GIRN                     GIRN             
	            VIRL                     VIRL                     VIRL              
	           NIRL                     NIRL                     NIRL               
	          RIVO                     RIVO                     RIVO                
	         MOXA                     MOXA                     MOXA                 
	        ROSE                     ROSE                     ROSE                  
	       POTT                     POTT                     POTT                   
	      TUTU                     TUTU                     TUTU                    
	     EURO                     EURO                     EURO                     
	    OUTS                     OUTS                     OUTS                     O
	   TWOS                     TWOS                     TWOS                     TW
	  LWEI                     LWEI                     LWEI                     LWE
	 OYES                     OYES                     OYES                     OYES
	ZYME                     ZYME                     ZYME                     ZYME 

	Leftover tiles:
	EQ
```

Shorter ladders take (up to exponentially) less time to calculate.

Finding the first ladder also works, but takes longer (the "A" tiles get used up quickly so the end of the ladder is harder to complete).

Simply delete `reverse $` from the following line in the main block:

``` haskell
      word4List = reverse $ getWord4List sortedWords
```

to obtain:

```
    UX                     ROUX                     ROUX                     ROUX   
    M                     PIUM                     PIUM                     PIUM    
                         QUOP                     QUOP                     QUOP     
                        MUTT                     MUTT                     MUTT      
                       COIT                     COIT                     COIT       
                      RIOT                     RIOT                     RIOT        
                     ZITI                     ZITI                     ZITI         
                    RIVO                     RIVO                     RIVO          
                   WINO                     WINO                     WINO           
                  WING                     WING                     WING            
                 RING                     RING                     RING             
                REEN                     REEN                     REEN              
               CELL                     CELL                     CELL               
              TODY                     TODY                     TODY                
             RYND                     RYND                     RYND                 
            KEEF                     KEEF                     KEEF                  
           BEDE                     BEDE                     BEDE                   
          GEED                     GEED                     GEED                    
         BEEF                     BEEF                     BEEF                     
        AEON                     AEON                     AEON                     A
       AALS                     AALS                     AALS                     AA
      AALS                     AALS                     AALS                     AAL
     AAHS                     AAHS                     AAHS                     AAHS
    AAHS                     AAHS                     AAHS                     AAHS 

    Leftover tiles:
    JV
```

It's easy to extend the code to longer words:

``` haskell
    data Word = Word Char Char Char Char Char Char deriving (Show, Ord, Eq)
    (!) :: Word -> Int -> Char
    (!) (Word a _ _ _ _ _) 0 = a
    (!) (Word _ b _ _ _ _) 1 = b
    (!) (Word _ _ c _ _ _) 2 = c
    (!) (Word _ _ _ d _ _) 3 = d
    (!) (Word _ _ _ _ e _) 4 = e
    (!) (Word _ _ _ _ _ f) 5 = f
    wordLen = 6 :: Int

    toString :: Word -> String
    toString (Word a b c d e f) = [a,b,c,d,e,f]

    fromString :: String -> Word
    fromString [a,b,c,d,e,f] = Word a b c d e f
```

Leading to:

```
                   TEFLON                   TEFLON                   TEFLON         
                  TOXOID                   TOXOID                   TOXOID          
                 JAWING                   JAWING                   JAWING           
                CICALE                   CICALE                   CICALE            
               GAVAGE                   GAVAGE                   GAVAGE             
              BOVINE                   BOVINE                   BOVINE              
             FOLIES                   FOLIES                   FOLIES               
            TOILER                   TOILER                   TOILER                
           YARTAS                   YARTAS                   YARTAS                 
          RIMIER                   RIMIER                   RIMIER                  
         DEDANS                   DEDANS                   DEDANS                   
        PEDANT                   PEDANT                   PEDANT                   P
       HAPUKU                   HAPUKU                   HAPUKU                   HA
      OUREBI                   OUREBI                   OUREBI                   OUR
     OWNERS                   OWNERS                   OWNERS                   OWNE
    ZYTHUM                   ZYTHUM                   ZYTHUM                   ZYTHU

    Leftover tiles:
    EQ
```

I chose to use a `Word` datatype, rather than aliasing `String`, for stricter type-checking on word length and faster random access. Not sure it was worth the hassle.
