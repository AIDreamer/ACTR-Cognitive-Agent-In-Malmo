(clear-all)

(define-model tutor-model
    
(sgp :esc t :lf .05 :trace-detail medium)


;; CHUNK-TYPES

(chunk-type addition-fact addend1 addend2 sum)
(chunk-type add-pair one1 ten1 one2 ten2 one-ans ten-ans carry)

;; CHUNKS

(add-dm
 ; The facts
 (fact34 ISA addition-fact addend1 3 addend2 4 sum 7)
 (fact67 ISA addition-fact addend1 6 addend2 7 sum 13)
 (fact103 ISA addition-fact addend1 10 addend2 3 sum 13)
 (fact17 ISA addition-fact addend1 1 addend2 7 sum 8)

 ; The goal
 (g1 ISA add-pair one1 6 ten1 3 one2 7 ten2 4)
)

;; Add productions here

(p start-pair

   =goal>
   ISA add-pair
   one1 =num1
   one2 =num2
   one-ans nil

   ==>
   =goal>
   one-ans busy

   +retrieval> ;Make retrieval request
   ISA addition-fact
   addend1 =num1
   addend2 =num2
)

(p add-ones
   
   =goal>
   ISA add-pair
   one-ans busy
   one1 =num1
   one2 =num2

   =retrieval>
   ISA addition-fact
   addend1 =num1
   addend2 =num2
   sum     =sum

   ==>
   =goal>
   one-ans =sum
   carry   busy

   +retrieval> ;Make retrieval request
   ISA addition-fact
   addend1 10
   sum     =sum
)

(p process-carry

   =goal>
   ISA add-pair
   ten1 =num1
   ten2 =num2
   one-ans =sum
   carry busy
   
   =retrieval>
   ISA addition-fact
   addend1 10
   addend2 =rem
   sum     =sum
   
   ==>
   =goal>
   ISA add-pair
   carry   1
   one-ans =rem
   ten-ans busy

   +retrieval>
   ISA addition-fact
   addend1 =num1
   addend2 =num2
)

(p no-carry

   =goal>
   ISA add-pair
   ten1 =num1
   ten2 =num2
   one-ans =ones
   carry busy
   
   ?retrieval>
   buffer failure

   ==>
   =goal>
   carry nil
   ten-ans busy
   
   +retrieval>
   ISA addition-fact
   addend1 =num1
   addend2 =num2
)

(p add-tens
   
   =goal>
   ISA add-pair
   ten-ans busy
   carry nil

   =retrieval>
   ISA addition-fact
   addend1 =num1
   addend2 =num2
   sum     =sum

   ==>
   =goal>
   ten-ans =sum
)

(p add-tens-carry
    =goal>
    ISA add-pair
    ten-ans busy
    carry   1
    
    =retrieval>
    ISA addition-fact
    sum =sum
    
    ==>
    =goal>
    carry nil

    +retrieval>
    ISA addition-fact
    addend1 1
    addend2 =sum
)

(goal-focus g1)
)
