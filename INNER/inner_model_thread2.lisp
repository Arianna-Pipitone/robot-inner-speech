(clear-all)

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(setq *sentence* " ")

(define-model inner

  ;(sgp :rt 10)
  ;(sgp :v t :act nil :esc t :lf .63 :mas 1.6 :ga 1.0 :imaginal-activation 1.0) 
  ;(sgp :style-warnings nil)
 
  ;(chunk-type comprehend-sentence relation arg1 arg2)
  
  ;(chunk-type comprehend-vo verb object)
  (chunk-type comprehend-voo verb object adverb location)
  (chunk-type relation kind arg1 arg2 symb)
  (chunk-type goal-state state)
  (chunk-type meaning word sense pos act)

  (chunk-type inner-where obj place)
  (chunk-type inner-etiquette-question pos obj1 obj2 symb)

  (add-dm
   (goal ISA goal-state state start)

   (p1 ISA comprehend-voo verb give object napkin adverb nil location nil)
   (p2 ISA relation kind in arg1 napkin arg2 box symb "The napkin is in the box")
   (p3 ISA inner-etiquette-question pos on obj1 napkin obj2 plate symb "The napkin has to stay on the plate")
   (p4 ISA inner-where obj napkin place basket)
   (p5 ISA comprehend-voo verb put object napkin adverb on location plate)
  

   (give ISA meaning word "give" sense give pos verb act "pick")
   (put ISA meaning word "put" sense put pos verb act "place")
   (pick ISA meaning word "pick" sense pick pos verb act "pick")
   (napkin ISA meaning word "napkin" sense napkin pos noun act nil)
   (basket ISA meaning word "basket" sense basket pos noun act nil)
   (in ISA meaning word "in" sense in pos adv)
   (on ISA meaning word "on" sense on pos adv)
   (near ISA meaning word "near" sense near pos adv)
   (left-adv ISA meaning word "left" sense left pos adv)
   (right-adv ISA meaning word "right" sense right pos adv)
   (plate ISA meaning word "plate" sense plate pos noun)
   (table ISA meaning word "table" sense table pos noun)
  

   (start ISA chunk)(detected-command-sound isa chunk)
   (detected-object-sound isa chunk) (encoded-command isa chunk)
   (start-inner isa chunk) (retrieving-meaning isa chunk)
   (encoded-obj isa chunk) (etiquette-question isa chunk) (encoded-location isa chunk)

   (verb ISA chunk)(noun ISA chunk)(adv ISA chunk)

     )


  (P detected-command-sound
    
    =goal>
    state    start

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    ?imaginal>
        state free
    ==>
    +imaginal>

    +aural>
      event =aural-location
    =goal>
     state     detected-command-sound
   
    )

  (P detected-object-sound
    
    =goal>
    state     encoded-command

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    
    ==>
    
    +aural>
      event =aural-location

    =goal>
     state     detected-object-sound
   
    )

  (P detected-adverbial-sound
    
    =goal>
    state     encoded-obj

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    =imaginal>
    ==>
    =imaginal>
    +aural>
      event =aural-location

    =goal>
     state  detected-adverbial-sound
   
    )

(P detected-location-sound
    
    =goal>
    state     encoded-adverb

    =aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
    =imaginal>
    ==>
    =imaginal>
    +aural>
      event =aural-location

    =goal>
     state  detected-location-sound
   
    )

      
  
   (P retrieve-meaning-verb
     =goal>
       state      detected-command-sound
     
     =aural>
        ISA        sound
        content    =word

     ==>
      +retrieval>
        ISA         meaning
        word        =word
        pos         verb
    )

   (P retrieve-meaning-object

     =goal>
     state    detected-object-sound
     =aural>
        ISA        sound
        content    =word
     ==>
      +retrieval>
        ISA         meaning
        word        =word
        pos         noun
    )

   (P retrieve-meaning-location

     =goal>
     state    detected-location-sound
     =aural>
        ISA        sound
        content    =word
     =imaginal>
     ==>
     =imaginal>
      +retrieval>
        ISA         meaning
        word        =word
        pos         noun
        )
    
    (P retrieve-meaning-adverb

     =goal>
     state    detected-adverbial-sound
     =aural>
        ISA        sound
        content    =word
     =imaginal>
     ==>
     =imaginal>
      +retrieval>
        ISA         meaning
        word        =word
        pos         adv
    )

 
 (P encode-command

     =goal>
       state      detected-command-sound

     =retrieval>
       word      =word
       act       =act
       pos      verb

     =imaginal>
        ISA         comprehend-voo
        verb        nil

     ;?vocal>
        ;state    free
     ==>

     =imaginal>
        verb        =retrieval

     +aural-location>
        ISA      audio-event
        kind     word
        location external
    !eval! (setq *sentence* (concatenate 'string "I have to " =act))
    ;+vocal>
     ;isa speak
     ;string =sentence
     !output! (I have to =act)
      

    =goal>
     state          encoded-command
    
 )


  (P encode-object

     
     =goal>
       state      detected-object-sound
    
     =retrieval>
       word      =word
       pos      noun

     =imaginal>
        ISA       comprehend-voo
        verb      =action
        object    nil
        
     ;?vocal>   
        ;state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* " " =word))
     ==>
     !bind! =sentence *sentence*
      ;+vocal>
        ;cmd         speak
        ;string      =sentence
     !output!    (Devo =action =word)
      
      =imaginal>
        object      =retrieval
      
      +goal>
        state    encoded-obj
      
    )


  (P encode-location

     
     =goal>
       state      detected-location-sound
    
     =retrieval>
       word      =word
       pos      noun
     =imaginal>
         ISA comprehend-voo
         verb =action
         object =obj
         adverb =adv
         location nil
     ?vocal>   
        state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* =word))
     ==>
      !bind! =sentence *sentence*
      +vocal>
        cmd         speak
        string      =sentence
        !output!    (Devo =action =obj =word)
      
      =imaginal>
        location      =retrieval
      
      +goal>
        state    encoded-location
      
    )

  (P encode-adverb

     
     =goal>
       state      detected-adverbial-sound
    
     =retrieval>
       word      =word
       pos      adv
     =imaginal>
         ISA comprehend-voo
         verb =action
         object =obj
         adverb nil
     ;?vocal>   
       ; state       free 
     !eval! (setq *sentence* (concatenate 'string *sentence* =word " the "))
     ==>
      ;!bind! =sentence *sentence*
      ;+vocal>
       ; cmd         speak
        ;string      =sentence
        
      
      =imaginal>
        adverb      =retrieval
      
      +goal>
        state    encoded-adverb
      
    )

   
   (P retrieve-etiquette-question
    =goal>
    state       encoded-location

    =imaginal>
      object    =obj

    ?vocal>
      state free

    ==>
    =imaginal>

    +vocal>
    cmd speak
    string   "What does the etiquette require?"
    !output! (Cosa prevede il galateo per =obj)
    +retrieval>
       ISA      inner-etiquette-question 
       obj1     =obj
    =goal>
     state etiquette-question

    )

  (P retrived-etiquette-question
    =goal>
       state       etiquette-question
    =imaginal>
    =retrieval>
        obj1     =obj1
        pos      =pos
        obj2     =obj2
        symb     =symb
    ?vocal>
       state      free
    ==>
    +vocal>
    cmd speak
    string     =symb
    !output! (Il galateo prevede che l'oggetto =obj1 si trovi a =pos dell'oggetto =obj2)
    =retrieval>
    =imaginal>
    =goal>
    state eval-etiquette
 )
  
  (P inner-w-question
    =goal>
     state       eval-etiquette

    =imaginal>
      adverb    =adv
      location  =loc
    =retrieval>
      pos    =pos
      obj2   =obj2     
    ==>
    !eval! (setq *adv* (write-to-string (eq =pos =adv)))
    !eval! (setq *loc* (write-to-string (eq =loc =obj2)))
   
    =imaginal>
   
    +retrieval>
       ISA meaning
       sense =pos
    =goal>
       state    quering-etiquette-question
    
  )

  (P make-etiquette-question
  	=goal>
  	state quering-etiquette-question
  	=imaginal>
  	     object =obj
  	     location =loc
  	     adverb =adv
    =retrieval>
        word =word
    
    !eval! (string-equal *adv* "nil")
  ==>
    !eval! (setq *sentence* (concatenate 'string "It contravenes etiquette... The position has to be " =word))
    !bind! =sentence *sentence*
    
    =imaginal>
    +retrieval>
       obj1    =obj
     =goal>
     state answering-etiquette-question
    )

  (P answering-etiquette-question
    =goal>
    state    answering-etiquette-question

    =retrieval>
        obj2   =obj2
    =imaginal>
     
    ==>

    =imaginal>
    
    +retrieval>
       ISA meaning
       sense  =obj2
     =goal>
     state    answer-etiquette-question

  ) 

  (P answer-etiquette-question
    =goal>
    state    answer-etiquette-question

    =retrieval>
        word   =word
    =imaginal>
      location =loc
    ==>

    =imaginal>
    !eval! (setq *sentence* (concatenate 'string *sentence*  =word " and not "))
    +retrieval>
       ISA meaning
       sense  =loc
     =goal>
     state    complete-answer-etiquette-question

  ) 

  (P complete-answer-etiquette-question
   =goal>
  state    complete-answer-etiquette-question
  =retrieval>
  word =word
  ?vocal>
  state free
  ==>
  !bind! =sentence (concatenate 'string *sentence* =word)
  +vocal>
  cmd speak
  string =sentence
  =goal>
  state requiring-conf
  )
  
  (P required-confirmation
   =goal>
  state    requiring-conf

  ?vocal>
  state free
  ==>
  +vocal>
  cmd speak
  string "Would you like I do that action anyway?"
  =goal>
  state attending-conf
  )

  (P attending-conf
  	=goal>
  	state attending-conf
  	=aural-location>
       isa      audio-event
       kind     word
       location external
    ?aural>
       state   free
   ==>
   +aural>
      event =aural-location
   =goal>
   state detecting-conf

   )


  (P yes
   =goal>
      state    detecting-conf
  ?vocal>
    state free
  ==>
    +vocal>
    isa speak
    string "Ok, I do it for you!"
    
    )

  #|(p prepare-control-left
   =goal>
     state prepare-control-left
   =imaginal>
   verb =verb
   ?manual>
     state free
   ?vocal>
     state free
   ==>
    =imaginal>
    +manual>
    cmd prepare
    style punch
    hand left
    +vocal>
    isa speak
    string "I will use my left arm"
    +retrieval>
    ISA meaning
    sense =verb
  =goal>
   state execute-act-left
   )

  (p execute-act-left
   =goal>
     state execute-act-left
   =imaginal>
   =retrieval>
   act =word
   ;?manual>
    ; state free
   ?vocal>
     state free
   ==>
    =imaginal>

    ;+manual>
    ;cmd execute
    
    !bind! =sentence (concatenate 'string "I'm using my left arm to " =word " the object ")
    +vocal>
    isa speak
    string =sentence
  
   =goal>
   state end
   )



  (P not-in-box 
   =goal>
      state    evaluate-action

   =imaginal>

   =retrieval>
      ISA meaning
      word "table"

   ?vocal>
    state   free
  ==>
    =imaginal>

    +vocal>
    cmd   speak
    string "I have already picked that object! It is on the table"

    =goal>
    state end

  ) |#
   
    (goal-focus goal)


)