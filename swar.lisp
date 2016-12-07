; This file is ungoverned by the Cypherpunks' Anti-License
; Do with it as you will.

; This is to adapt the notation
; introduced at brightidea.com (which is now called "swar schema")
; to the graph representation schema introduced in utgraph.lsp

; http://www.brightidea.com/ob_view_idea.asp?idea_id={9CBDBD6F-BCB9-11D4-9F90-00A0C932F0D2}&bucket_id=
; http://geocities.com/n8chz/utgraph.txt
; http://geocities.com/n8chz/tweak.txt

; this maps the brightidea codes to the numbers
; representing connectivity in the ut graph.
; (graph as upper triangular matrix)

; We introduce what will be termed the "swar schema,"
; which is offered as a sort of markup language
; for proximity modeling.

; As with the "maxhi schema,"
; (http://geocities.com/n8chz/PubwanWiki/MaxhiSchema.html)
; the swar schema involves sorting objects into five categories.
; The swar schema is also like the maxhi schema
; in that one of the five states represents neutrality,
; with the other four being a cartesian product of pairs.
; The five states of the swar schema are as follows:

; "sa" (strong attraction)
; "sr" (strong repulsion)
; "wa" (weak attraction)
; "wr" (weak repulsion)
; nil (no attractive or repulsive force)

; In the swar/utgraph schema,
; graph vertices represent pubwan virtual objects (PVO's).
; (http://geocities.com/n8chz/pvo.htm)
; Swar entries represent edges of an utgraph,
; which in turn represent attraction or repulsion
; between PVO's.

; In this file, the representation of a swar entry
; is as follows:

; '("pvo1-name" "sa" "pvo2-name")
; indicating that pvo strongly attracts pvo2.

; In the original brightidea article,
; repulsive forces are represented
; by positive numbers, and attractive by negative.
; This reflects the idea that
; the second derivative of distance with
; respect to time is negative if
; two objects are accelerating
; toward each other, and of course
; force is directly proportinal to acceleration.

; Since we are adapting the swar schema
; to a graph theoretic model,
; we will use the convention in graph theory
; of representing adjacency (akin to attraction, no?) with positive numbers.
; Hopefully this will not cause too much confusion.

; Without further adieu, here is our first functional code:

(defun swar->adj (swar)
 (cdr (assoc swar '(("sa" . 1) ("wa" . 1/2) (nil . 0) ("wr" . -1/2) ("sr" . -1)) :test #'string=)))

; Likewise, a whole graph can be represented by a list of swar entries.

(defun swarlist->vertexlist (swarlist)
 (do
  ((left swarlist (cdr left))
   (result nil (cons (caar left) (cons (caddar left) result))))
  ((null left) (remove-duplicates result :test #'string=))))

; The 'utpair' is a list
; of the C(n,2) pairings of
; items in 'nodelist'

; list C(length(nodelist),2) distinct pairings of nodelist
; this results in a utmatrix not entirely unlike the
; familiar "mileage" (i.e. distance) tables
; on an "obsolete" technology called printed maps

; The order in which the pairings are presented is as follows:

; n-1,n
; n-2,n
; n-2,n-1
; n-3,n
; ...
; 1,n
; ...
; 1,3
; 1,2

; In this project, the nodelist will be the
; vertex list derived by swarlist->vertexlist

(defun utpair (nodelist)
 (let
  ((result nil))
  (do
   ((i nodelist (cdr i)))
   ((null (cdr i)) result)
   (do
    ((j (cdr i) (cdr j)))
    ((null j) result)
    (setf result (cons (cons (car i) (car j)) result))))))

(defun swapcons (c)
 (cons (cdr c) (car c)))

(defun utequiv (x y)
 (or (equal x y) (equal (swapcons x) y)))

(defun swar->adjacency (swar)
 (cons (car swar) (caddr swar)))

(defun pairswar? (pair swar)
 (utequiv pair (swar->adjacency swar)))

; Each utpairing represents a pair of vertices,
; which is to say a potential edge.
; To express a list of swar entries
; in utgraph form, the pairs must be
; replaced by numbers representing adjacencies/forces.
; The following function does this for a single utpairing:

(defun utpairing->adjacency (pair swarlist)
 (swar->adj (cadar (member pair swarlist :test #'pairswar?))))

; Here we replace the entire list of pairs
; with a list of adjacencies

(defun utlist->adjlist (utlist swarlist)
 (mapcar
  #'(lambda (x) (utpairing->adjacency x swarlist))
  utlist))

; partition the interval [0,1] using the midpoint rule

(defun midpoint-partition (n)
 (let
  ((recip (/ n))
   (val nil))
  (do
   ((q (- (1- (/ recip 2))) (- q recip)))
   ((minusp q) val)
   (setf val (cons q val)))))

; give a random permutation of a liszt

(defun random-permutation (liszt)
 (let
  ((val nil)
   pick)
  (do
   ((left liszt (remove pick left :test #'equal)))
   ((null left) val)
   (setf
    pick (nth (random (length left)) left)
    val (cons pick val)))))

; transpose a matrix which is represented as a list of lists

(defun transpose (lol)
 (if (and lol (car lol))
  (cons
   (mapcar #'car lol)
   (transpose (mapcar #'cdr lol)))))

; With this, we have the list of adjacencies
; To complete the utgraph structure
; we need an initial embedding.
; We start with a random embedding,
; and then apply our embedding algorithm.

; Here is our random embedding function:

(defun random-init-embed (n &optional (d 3))
 (do
  ((left d (1- left))
   (val nil (cons (random-permutation (midpoint-partition n)) val)))
  ((zerop left) (transpose val))))


; Now we put it all together to form an utgraph structure.

(defun swarlist->utgraph (swarlist)
(let (n)
 (cons
  (utlist->adjlist
   (utpair
    (setf n (swarlist->vertexlist swarlist)))
   swarlist)
  (random-init-embed (length n) 2))))

; Here is another random-valued function.
; This one is just for testing the routines
; we've coded so far.

(defun random-node-name (lenth &optional exclude)
 (let (try)
  (if
   (string= (setf try (format nil "~D" (random lenth))) exclude)
   (random-node-name lenth exclude)
   try)))
   

(defun random-force nil
 (case (random 5)
  (0 nil)
  (1 "sa")
  (2 "sr")
  (3 "wa")
  (4 "wr")))

(defun random-swar (lenth)
 (let (furst)
  (list
   (setf furst (random-node-name lenth))
   (random-force)
   (random-node-name lenth furst))))

(defun random-swarlist (lenth)
 (let ((result nil))
  (dotimes (i lenth result)
   (setf result (cons (random-swar lenth) result)))))
  

(defun swartest nil
 (do
  ((left 10 (1- left)))
  ((zerop left) t)
  (print (swarlist->utgraph (random-swarlist (1+ (random 20)))))))

; run the test

(setf a (random-swarlist 30))
(setf b (swarlist->vertexlist a))
(setf c (utpair b))
(setf d (utpairing->adjacency (car c) a))
(setf e (utlist->adjlist c a))
(swartest)
