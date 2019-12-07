;;;; -*- Mode: Lisp; -*- 
;;;; Team Members: <Jason Drucker, Qinyi Wu, Colin Naar>
;;;;
;;;;
;;;; Submission Deadline: Sunday, December 8, 11:59:59pm
;;;; Report Deadline: Monday, December 9, 11:59:59pm
;;;; 
;;;; Please submit your code as a .lisp file in Blackboard.
;;;;
;;;;



;;;HELPFUL TOPLEVEL HINTS:

;;; To run your program, first load it into the LispWorks Editor,
;;; then click on the "Compile Buffer" button on the top of the
;;; Editor window.  Then, in the LISTENER, you can call any of
;;; the functions in the program file.
;;;
;;; If you find yourself in an error, and you want to know how
;;; you got to that point in the program, when the textual debugger
;;; message appears, you can click on the "Debug" button at the
;;; top of the LISTENER window.  You'll get a GUI debugger that
;;; will show you things like the call stack, the values of the
;;; local variables, and, if you click on a call stack entry,
;;; if the entry corresponds to a function in your program, the
;;; Editor will immediately jump you to the line of code where
;;; the error occured.  Ask in class if you have trouble with
;;; this neat feature. 


;;;HELPFUL PROGRAMMING HINTS:
;;; To create a person structure, use the automatically-generated
;;; function "make-person" as follows:
;;;
;;; (make-person :name xxx :parent1 yyy :parent2 zzz)
;;;
;;; where "xxx" is the string or symbol (or a variable holding it)
;;; for the name of the person, "yyy" is the string or symbol for
;;; the name of the person's first parent, and "zzz" is of course
;;; the name of the person's second parent.
;;;
;;; for example, to store a new person in a variable p, use this:
;;;
;;; (SETF p (make-person :name "Barbara" :parent1 "Fred" :parent2 "Carol"))
;;;
;;;

;;; The DEFSTRUCT function tells Lisp to autmatically create
;;; getter functions for each slot.  Their names are based on
;;; the names of the slots:
;;;
;;;  "person-name" will get the value stored in the NAME slot
;;;  "person-parent1" will get the value in the PARENT1 slot
;;;

;;; The LOOP function (macro) is used to iterate in many ways.
;;; Here are some examples:
;;;
;;; (LET ((newlist nil)
;;;       (mylist (LIST 1 2 3 4 5 6 7 8)))
;;;   (LOOP for i in mylist DOING
;;;     (SETF newlist (APPEND newlist (LIST (+ i 1)))))
;;;   newlist)
;;;
;;;  The above will make a new list that contains
;;;  numbers that are one more than their corresponding
;;;  elements in mylist.  Notice that the new sum is added
;;;  at the END of the growing new list!
;;;  This could also be done more elegantly in Lisp using
;;;  a nameless lambda function
;;;
;;;  (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;    (MAPCAR #'(lambda (x) (+ x 1)) mylist))
;;;
;;; MAPCAR applies its first argument (a function) to each
;;; element in the second argument (a list), and collects
;;; the results of all the function calls into a new list
;;; and returns that list.
;;;
;;; Here is another LOOP example that does the same thing:
;;;
;;; (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;  (LOOP for x in mylist collecting
;;;     (+ x 1)))
;;;


;;;-------------------------------
;;;PROJECT CODE STARTS HERE.
;;;-------------------------------

(DEFSTRUCT (person
            (:print-function print-person))
  (parent1 NIL) ; a symbol or string or NIL
  (parent2 NIL) ; a symbol or string or NIL
  (name NIL)
  (children (LIST))
)   ; a symbol or string or NIL


;;If you want to add more slots to the person
;;structure (say, children or spouse), use
;;the same syntax as you see above for the slots
;;to add them to the above definition.
;;It is likely that if you add a "children" slot,
;;it will hold a list or array of children names
;;rather than a single atom.



;;NOTE: This function is complete, no need to change it unless you
;;want to update it to show other slots you add to the person struct
;;definition.
(DEFUN print-person (item stream depth)
  "A helper function for Lispworks to be able to show you what is
in a person structure concisely."
    (DECLARE (IGNORE depth))
    (FORMAT stream "#<P name:~S p1:~S p2:~S>"
            (person-name item) (person-parent1 item) (person-parent2 item))
    item)


;;;NOTE: This function is complete. No need to change it.
(DEFUN lookup-person (name tree)
  "Returns a PERSON structure corresponding to the key NAME in the hashtable TREE.
NAME must be a STRING or a SYMBOL. If there is no one in the tree with the name
in NAME, returns NIL."
  (GETHASH name tree nil))


;;;NOTE: This function is complete. No need to change it.
(DEFUN person-exists (name tree)
  "Returns T when the key NAME has an actual person struct stored in TREE.
Returns NIL (false) otherwise."
  (WHEN (lookup-person name tree)
    t))


;;;NOTE: This function is complete. No need to change it.
(DEFUN get-ancestors (name tree)
  "Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does dynamic type checking
to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "ANCESTORS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "ANCESTORS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)   
    (ancestorsb name tree)))

;;;------------------------------------------------
;;; TEAM SHOULD PUT ALL NEW HELPER FUNCTION
;;; DEFINITIONS BELOW THIS COMMENT
;;;------------------------------------------------ 

;;This function needs to be defined by your team.
(DEFUN ancestorsb (name tree)
  "A helper function for the ANCESTORS function. 
Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does not check if NAME 
exists as a person in the TREE!"
  (LET* ((p (lookup-person name tree))
         (par1 (person-parent1 p))
         (par2 (person-parent2 p)))
    (when par1
      (append (list par1 par2)
              (ancestorsb par1 tree)
              (ancestorsb par2 tree)))
    ))

(DEFUN get-ancestors-on-level (name tree level) ;; Maybe need to check for nil here
  (WHEN (>= level -1)
    (LET*(
         (p (lookup-person name tree))
         (par1 (person-parent1 p))
         (par2 (person-parent2 p))
         (newlev (- level 1))
         (ret (LIST))
         )
           (when(eq level -1)
            (push name ret)
            )
          (when par1
            (setq ret (append 
            ret
            (get-ancestors-on-level par1 tree newlev)
            (get-ancestors-on-level par2 tree newlev)
          )))
          ret
        )
    ))

(DEFUN get-ancestors-up-to-level (name tree level)
  (WHEN (>= level 0)
    (LET*(
         (p (lookup-person name tree))
         (par1 (person-parent1 p))
         (par2 (person-parent2 p))
         (newlev (- level 1))
         (ret (LIST))
         )
      (push name ret)
      (when par1
            (setq ret (append 
            ret
            (get-ancestors-up-to-level par1 tree newlev)
            (get-ancestors-up-to-level par2 tree newlev)
          )))
      ret
      )
))


(DEFUN get-cousins-unmod (name tree level)
  (LET(
       (topancestors (get-ancestors-on-level name tree level))
       (famlinelist (get-ancestors-up-to-level name tree level))
       (ret (LIST))
       )
    (LOOP for anc in topancestors DOING 
          (setq ret (append ret (get-descendants-on-level anc tree level famlinelist)))
          )
    (remove-duplicates ret :test #'equal)
    )
  )


(DEFUN get-cousins-mod (name tree level)
  (LET*(
        (ancestors (get-ancestors name tree))
        (cousins (get-cousins-unmod name tree level))
        (ret (get-cousins-unmod name tree level))
        )
    (LOOP for anc in ancestors DOING 
          (setq ret (append ret (get-cousins-unmod anc tree level)))
          )
    (LOOP for c in cousins DOING
          (setq ret (append ret (get-descendants c tree)))
        )
    (remove-duplicates ret :test #'equal)
    )
)

(DEFUN get-descendants-on-level (name tree level familylinelist)
(WHEN(>= level -1)
  (LET*(
       (children (get-children name tree))
       (templist (LIST))
       (newlev (- level 1))
       (ret (LIST))
       )
    (when(eq level -1)
            (push name ret)
            )
    (WHEN children
      (LOOP for child in children DOING
            (WHEN (eq (is-member child familylinelist) 'No)
              (setq ret (append ret (get-descendants-on-level child tree newlev familylinelist)))
            )
      ))
    ret
    ))
)

(DEFUN get-descendants (name tree)
   (LET* (
         (children (get-children name tree))
         (templist (LIST))
         )
    (when children
      (setq templist (append templist children))
      (loop for child in children DOING
            (setq templist (append templist (get-descendants child tree)))
          ))
    (remove-duplicates templist :test #'equal)
))

(DEFUN get-unrelated (name tree)
  (LET(
       (anc (get-ancestors name tree))
       (des (LIST))
       (treelist (LIST))
       )
    (WHEN anc 
      (loop for a in anc DOING 
            (setq des (append des (get-descendants a tree)))
      )
      (setq des (append des anc))
      (remove-duplicates des :test #'equal)
      (loop for k being each hash-key of tree DOING
            (WHEN (eq (is-member k des) 'No)
                   (push k treelist))
      )
)treelist)
)


(DEFUN get-children (name tree)
  (LET*(
       (p(lookup-person name tree))
       (pchldrn (person-children p))
      )pchldrn)
)

(DEFUN get-siblings (name tree)
  (LET*(
       (p(lookup-person name tree))
       (parent1 (person-parent1 p))
       (parent2 (person-parent2 p))
       (chil1 (get-children parent1 tree))
       (chil2 (get-children parent2 tree))
       (namevar (person-name p))
       )
     (remove namevar (remove-duplicates (append chil1 chil2) :test #'equal) :test #'equal)
   )
)

;; ALL IS METHODS THAT WANT YES OR NO RETURNED SHOULD CALL THIS
(DEFUN is-member(n l)
  (LET(
       (ret 'No)       
       )
    (WHEN (member n l :test #'equal)
      (setq ret 'Yes)
    )
ret
))

(DEFUN is-cousin(cousin person tree level)
  (is-member cousin (get-cousins-mod person tree level))
)

(DEFUN is-ancestor(ancestor person tree)
  (is-member ancestor (get-ancestors person tree))
)

(DEFUN is-child (cname pname tree)
  (is-member cname (get-children pname tree))  ;; ALL IS METHODS SHOULD JUST CALL IS MEMBER
)

(DEFUN is-unrelated (name1 name2 tree)
  (is-member name1 (get-unrelated name2 tree))
)

(DEFUN is-sibling (p1 p2 tree) 
  (is-member p1 (get-siblings p2 tree))
)

(DEFUN add-person (name struct tree)
  "This should enter the person structure in STRUCT into
the hashtable in TREE with the key in NAME."
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "STORE-PERSON called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (NOT (person-p struct))
    (ERROR "STORE-PERSON called with STRUCT (~A) that is not a PERSON structure." struct))
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "STORE-PERSON called with NAME (~A) that is not a SYMBOL or a STRING." name))
  ;; NOTE1: TEAMS NEED TO WRITE THE NEXT LINE.
  ;;        Hint: a "setf" expression.
  (SETF (GETHASH name tree) struct) ;;BY ME
  ;; NOTE2: Leave this last line as "name" so
  ;;        that the name argument is what is
  ;;        returned by this function.
  name)


(DEFUN handle-E (linelist tree)
  (setf linelist (append (list "E") linelist))
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET (
        (len (list-length linelist))
        (p1 (nth 1 linelist))
        (pers1 nil)
        (p2 (nth 2 linelist))
        (pers2 nil)
        (p3 (nth 3 linelist))
        (pers3 nil)
        (people (list p1 p2)) ; only contains parents
        )
          (WHEN (eql (lookup-person p1 tree) NIL)
            (setf pers1 (add-person p1 (make-person :name p1) tree))
            )
          (WHEN (eql (lookup-person p2 tree) NIL)
            (setf pers2 (add-person p2 (make-person :name p2) tree))
            )
          (setf pers1 (lookup-person p1 tree))
          (setf pers2 (lookup-person p2 tree))
          (WHEN (NOT (eql p3 nil))
            (add-person p3 (make-person :name p3 :parent1 p1 :parent2 p2) tree)
            (setf (person-children pers1) (append (get-children p1 tree) (list p3)))
            (setf (person-children pers2) (append (get-children p2 tree) (list p3)))
            )
          )
)
    
  
(DEFUN handle-X (linelist tree)
  (setf linelist (append (list "X") linelist))
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET (
        (p1 (nth 1 linelist))
        (rel (nth 2 linelist))
        (p2 (nth 3 linelist))
        (level nil)
        (ret nil)
    )
    (WHEN (string= rel "cousin")
      (setf level (parse-integer p2))
      (setf p2 (nth 4 linelist))
       (WHEN (NOT (person-exists p2 tree))
      (print "Person does not exist in tree")
      (terpri) 
      (terpri)
      (return-from handle-X nil)
      )
       (IF (= level 0)
          (setf ret (is-sibling p1 p2 tree))
          (setf ret (is-cousin p1 p2 tree level))
        )
      (setf ret (is-cousin p1 p2 tree level))
      )
    (WHEN (NOT (person-exists p2 tree))
      (print "Person does not exist in tree")
      (terpri) 
      (terpri)
      (return-from handle-X nil)
      )
    (WHEN (string= rel "child")
      (setf ret (is-child p1 p2 tree))
     )
    (WHEN (string= rel "sibling")
      (setf ret (is-sibling p1 p2 tree))
     )
    (WHEN (string= rel "ancestor")
      (setf ret (is-ancestor p1 p2 tree))
     )
    (WHEN (string= rel "unrelated")
      (setf ret (is-unrelated p1 p2 tree))
     )
(print ret)
(terpri)
(terpri)
))

(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (setf linelist (append (list "W") linelist))
  (LET (
        (rel (nth 1 linelist))
        (p2 (nth 2 linelist))
        (level nil)
        (ret nil)
        )
  
    (WHEN (string= rel "cousin")
      (setf level (parse-integer p2))
      (setf p2 (nth 3 linelist))
      (WHEN (NOT (person-exists p2 tree))
        (print "Person does not exist in tree")
        (terpri) 
        (terpri)
        (return-from handle-W nil)
        )
      (IF (= level 0)
          (setf ret (get-siblings name tree))
          (setf ret (get-cousins-mod p2 tree level))
        )
      )
    (WHEN (NOT (person-exists p2 tree))
    (print "Person does not exist in tree")
    (terpri) 
    (terpri)
    (return-from handle-W nil)
    )
     (WHEN (string= rel "child")
      (setf ret (get-children  p2 tree))
     )
    (WHEN (string= rel "sibling")
      (setf ret (get-siblings p2 tree))
     )
    (WHEN (string= rel "ancestor")
      (setf ret (get-ancestors p2 tree))
     )
    (WHEN (string= rel "unrelated")
      (setf ret (get-unrelated p2 tree))
     )
    (print-on-seperate-lines ret)
    (terpri)
    )
    )

(DEFUN print-on-seperate-lines(list )
  (terpri)
  (LOOP for element in list DOING 
        ;(print element) 
        (format t "~a~%" element)
  )
)

(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal))
        (retend "END")
)
    (LOOP     
      (COND
         ((STRING= (FIRST line-items) "E") (handle-E (REST line-items) tree))
         ((STRING= (FIRST line-items) "W") (format t "~{~a ~}" line-items)(handle-W (REST line-items) tree))
         ((STRING= (FIRST line-items) "X") (format t "~{~a ~}" line-items)(handle-X (REST line-items) tree))
         (t (RETURN nil))) ; end of file reached
      (SETF line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
retend))

;;(family (open "/Users/jasondrucker/Desktop/Programming\ Languages/TestInputs/test2.txt"))
