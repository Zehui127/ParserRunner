#|----------------------------------------------------------------------------|
 | Copyright 2006, 2017 Rebecca Watson, Oeistein Andersen                     |
 |                                                                            |
 | This file is part of RASP.                                                 |
 |                                                                            |
 | RASP is free software: you can redistribute it and/or modify it            |
 | under the terms of the GNU Lesser General Public License as published      |
 | by the Free Software Foundation, either version 3 of the License, or       |
 | (at your option) any later version.                                        |
 |                                                                            |
 | RASP is distributed in the hope that it will be useful,                    |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of             |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              |
 | GNU Lesser General Public License for more details.                        |
 |                                                                            |
 | You should have received a copy of the GNU Lesser General Public License   |
 | along with RASP.  If not, see <http://www.gnu.org/licenses/>.              |
 |----------------------------------------------------------------------------|#
;;; PROB CODE -XML- LSP
;;;
;;; Copyright: Rebecca Watson April 2006

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

(defparameter +xml-print-dtd+ nil)
(defparameter +xml-print-enc+ nil)
(defparameter +xml-print-rasp-tags+ nil)

(defun set-print-options (arg-p)
  (defun arg-p (flag) 
    (if (find flag arg-p) t nil))
  (setq +print-desc+ (not (arg-p #\s)))
  (setq +compress-xml+ (not (arg-p #\h)))
  (setq +replace-circ+ (not (arg-p #\c)))
  (setq +xml-print-dtd+ (arg-p #\d))
  (setq +xml-print-enc+ (arg-p #\e))
  (setq +xml-print-rasp-tags+ (arg-p #\r)))

(defparameter +start-level+ 0)

(defparameter +compress-xml+ t)

(defun print-xml-tabs (out-str string level &optional (newline t))
  (when (and (null +compress-xml+) newline)
    ;;(terpri)
    (format out-str "~&")
    (dotimes (n (+ level +start-level+))
      ;;(format t "   ")
      (format out-str "   ")))
  (format out-str "~A" string))

(defun xml-escaped-output (str)
  (let ((xml-string "")
	(char-list (coerce (format nil "~A" str) 'list))
	c)
    (dotimes (i (length char-list))
      (setq c (elt char-list i))
      (cond ((char= #\" c) (setq xml-string (concatenate 'string xml-string "&quot;")))
	    ((char= #\' c) (setq xml-string (concatenate 'string xml-string "&apos;")))
	    ((char= #\& c)
	     (if (or (and (>= (length str) (+ 10 i))
			  (equal (subseq str i (+ 10 i)) "&raspcirc;"))
		     (and (>= (length str) (+ 10 i))
			  (equal (subseq str i (+ 10 i)) "&raspsquo;"))
		     (and (>= (length str) (+ 11 i))
			  (equal (subseq str i (+ 11 i)) "&rasprsquo;")))
		 (setq xml-string (concatenate 'string xml-string "&"))
		 (setq xml-string (concatenate 'string xml-string "&amp;"))))
	    ((char= #\< c) (setq xml-string (concatenate 'string xml-string "&lt;")))
	    ((char= #\> c) (setq xml-string (concatenate 'string xml-string "&gt;")))
	    (t (setq xml-string (concatenate 'string xml-string (format nil "~A" c))))))
    ;; replace the &circ; with ^ chars:
    xml-string))

(defun start-xml (out-str)
  (when +xml-print-enc+
    (format out-str "<?xml version=\"1.0\" encoding=\"~A\"?>~%" +char-encoding-string+))
  (when  +xml-print-dtd+
    (format out-str "
<!DOCTYPE rasp [
<!ELEMENT rasp (sentence+)>
<!ELEMENT sentence (lisp-lemma-list?, lemma-list?, complexity?, nbest-parses?, lisp-weighted?, weighted?, lisp-ewg-weighted?, ewg-weighted?, xparse?)>
<!ATTLIST sentence num CDATA #REQUIRED>
<!ELEMENT lisp-lemma-list (#PCDATA)>
<!ELEMENT lemma-list (lemma*)>
<!ELEMENT lemma EMPTY>
<!ATTLIST lemma 
	wtag CDATA #IMPLIED
	lem CDATA #IMPLIED
	affix CDATA #IMPLIED
	num CDATA #IMPLIED
	wnum CDATA #IMPLIED
	pos CDATA #IMPLIED>


<!ELEMENT complexity (comp+) >
<!ELEMENT comp	EMPTY>
<!ATTLIST comp 
	name CDATA #REQUIRED
	value CDATA #REQUIRED >
	
<!ELEMENT xparse (#PCDATA)>

<!ELEMENT weighted (gr*)>
<!ELEMENT ewg-weighted (gr*)>

<!ELEMENT lisp-weighted (#PCDATA)>
<!ELEMENT lisp-ewg-weighted (#PCDATA)>

<!ELEMENT nbest-parses (parse-set+)>
<!ATTLIST nbest-parses num CDATA #REQUIRED>

<!ELEMENT parse-set (lisp-tree-rasp?, tree-rasp?, lisp-rmrs-rasp?, rmrs-rasp?, lisp-sparkle?, sparkle?, lisp-susanne?, susanne?, lisp-upenn?, upenn?, lisp-alias?, alias?, lisp-gr-list?, gr-list?) >
<!ATTLIST parse-set pnum CDATA #REQUIRED>
<!ATTLIST parse-set score CDATA #REQUIRED>

<!ELEMENT lisp-tree-rasp (#PCDATA)>
<!ELEMENT lisp-sparkle (#PCDATA)>
<!ELEMENT lisp-susanne (#PCDATA)>
<!ELEMENT lisp-upenn (#PCDATA)>
<!ELEMENT lisp-alias (#PCDATA)>
<!ELEMENT lisp-gr-list (#PCDATA)>

<!ELEMENT lisp-rmrs-rasp (rmrs)>

<!ELEMENT tree-rasp (tree)>
<!ELEMENT rmrs-rasp (rmrs)>
<!ELEMENT sparkle (tree)>
<!ELEMENT susanne (tree)>
<!ELEMENT upenn (tree)>
<!ELEMENT alias (tree)>

<!ELEMENT tree (node)*>
<!ELEMENT node (node | wnode)*>
<!ATTLIST node rule CDATA #REQUIRED>
<!ELEMENT wnode EMPTY>
<!ATTLIST wnode lemma CDATA #REQUIRED>
<!ELEMENT gr-list (gr*)>
<!ELEMENT gr (gr-weight?)>
<!ATTLIST gr 
	type CDATA #IMPLIED
	subtype CDATA #IMPLIED
	head CDATA #IMPLIED
	dep CDATA #IMPLIED
	init CDATA #IMPLIED >
<!ELEMENT gr-weight EMPTY>	
<!ATTLIST gr-weight weight CDATA #REQUIRED>

<!-- from rmrs.dtd in $RASP/mrs/rmrs/ -->

<!ELEMENT rmrs (label, (ep|rarg|ing|hcons)*)>
<!ATTLIST rmrs
          cfrom CDATA #REQUIRED
          cto   CDATA #REQUIRED 
          surface   CDATA #IMPLIED 
          ident     CDATA #IMPLIED >

<!ELEMENT ep ((realpred|gpred), label, var)>
<!ATTLIST ep
          cfrom CDATA #REQUIRED
          cto   CDATA #REQUIRED 
          surface   CDATA #IMPLIED
	  base      CDATA #IMPLIED >

<!ELEMENT realpred EMPTY>

<!ATTLIST realpred
          lemma CDATA #REQUIRED
          pos (v|n|j|r|p|q|c|x|u|a|s) #REQUIRED
          sense CDATA #IMPLIED >

<!ELEMENT gpred (#PCDATA)>

<!ELEMENT label EMPTY>

<!ATTLIST label 
          vid CDATA #REQUIRED >

<!ELEMENT var EMPTY>
<!ATTLIST var
          sort (x|e|h|u|l) #REQUIRED
          vid  CDATA #REQUIRED 
          num  (sg|pl|u) #IMPLIED
          pers (1|2|3|1-or-3|u) #IMPLIED
          gender (m|f|n|m-or-f|u) #IMPLIED
          divisible (plus|minus|u) #IMPLIED
          cogn-st (type-id|uniq-id|fam|activ|in-foc|uniq-or-less|uniq-or-fam|fam-or-activ|active-or-more|fam-or-less|uniq-or-fam-or-activ|fam-or-more|activ-or-less|uniq-or-more|u) #IMPLIED
          tense (past|present|future|non-past|u) #IMPLIED
          telic (plus|minus|u) #IMPLIED
          protracted (plus|minus|u) #IMPLIED
          stative (plus|minus|u) #IMPLIED
          incept (plus|minus|u) #IMPLIED
          imr (plus|minus|u) #IMPLIED
          boundedness (plus|minus|u) #IMPLIED
          refdistinct (plus|minus|u) #IMPLIED >


<!ELEMENT rarg (rargname, label, (var|constant))>

<!ELEMENT rargname (#PCDATA)>

<!ELEMENT constant (#PCDATA)>

<!ELEMENT ing (ing-a, ing-b)>
<!ELEMENT ing-a (var)>
<!ELEMENT ing-b (var)>

<!ELEMENT hcons (hi, lo)>
<!ATTLIST hcons 
          hreln (qeq|lheq|outscopes) #REQUIRED >

<!ELEMENT hi (var)>
<!ELEMENT lo (label|var)>
]>~%"))
  (when +xml-print-rasp-tags+
    (print-xml-tabs out-str "<rasp>" 0))
    )

(defun end-xml (out-str)
  (when +xml-print-rasp-tags+
    (print-xml-tabs out-str "</rasp>" 0)
    (terpri out-str)
    (force-output out-str)))

(defun xml-start-sentence-print (out-str sentence n weights &optional (level 1))
  (print-xml-tabs out-str 
		    (format nil "<sentence num='~A'>" *sentence-number*) 
		    level)
  ;; if any non-xml output then output the lemma list for lisp too:
  (when +print-lemma-lisp+
    (format out-str "~&<lisp-lemma-list><![CDATA[~A" *current-sentence*)
    ;;(print-xml-tabs out-str 
    ;;		  (format nil "<string><![CDATA[~A" *current-sentence*)
    ;;		  (1+ level))
    (cond
      ((> n (1+ +analysis-output-nprobs+))
       (format out-str "(~{~,3F~^ ~} ...)" ;; three decimal points on the float number
	       (butlast weights (- n +analysis-output-nprobs+))))
      ((> n 0)
       (format out-str "(~{~,3F~^ ~})" weights))
      (t (format out-str "()")))
    (format out-str  "]]></lisp-lemma-list>"))
  ;;(format t "WEIGHTS: (~{~,3F~^ ~})" weights)
  (setq *current-sentence* nil)
  (if (consp (car sentence))
      (let ((mtag-num -1))
	(dolist (mtag-list sentence)
	  (dolist (mtag (cdr mtag-list))
	    (incf mtag-num)
	    ;(let ((lem-items (mapcar #'xml-escaped-output (split-word (car mtag)))))
	    (let ((lem-items (split-word (car mtag))))
	     ;;(format t "split lemma:~A~%" lem-items)
	      (setf (elt lem-items 3) (format nil "~A" mtag-num))
	      (push (list (car mtag) lem-items) 
		    *current-sentence*)))))
      (dolist (stag sentence)
	;(let ((lem-items (mapcar #'xml-escaped-output (split-word stag))))
	(let ((lem-items (split-word stag)))
	  (push (list stag lem-items) *current-sentence*)
	  )
	)
      )
  ;;(pprint *current-sentence*)
  (setq *current-sentence* (nreverse *current-sentence*))
  ;; print the lemmas (wtag) lem (affix) number, pos
  (when (or (print-xml-fn-spec-p) +print-lemma-xml+)
    (format out-str "~&")
    (print-xml-tabs out-str "<lemma-list>" (1+ level))
    (let (next-lemma
	  lem-list)
      (dolist (lemma *current-sentence*)
	(setq next-lemma "<lemma")
	(setq lem-list (cadr lemma))
	(unless (equal "" (car lem-list))
	  ;(setq next-lemma (format nil "~A tag='~A'" next-lemma (car lem-list))))
	  (setq next-lemma (format nil "~A ~A" next-lemma (car lem-list))))
	(unless (equal "" (cadr lem-list))
	  (setq next-lemma (format nil "~A lem='~A'" next-lemma (xml-escaped-output (cadr lem-list)))))
	(unless (equal "" (caddr lem-list))
	  (setq next-lemma (format nil "~A affix='~A'" next-lemma (caddr lem-list))))
	(unless (equal "" (cadddr lem-list))
	  (setq next-lemma (format nil "~A num='~A'" next-lemma (cadddr lem-list))))
	(unless (equal "" (car (cddddr lem-list)))
	  (setq next-lemma (format nil "~A wnum='~A'" next-lemma (car (cddddr lem-list)))))
	(unless (equal "" (cadr (cddddr lem-list)))
	  (setq next-lemma (format nil "~A pos='~A'" next-lemma (xml-escaped-output (cadr (cddddr lem-list))))))
	(setq next-lemma (format nil "~A/>" next-lemma))
	;;(format t "next-lemma:~A~%" next-lemma)
	(print-xml-tabs 
	 out-str
	 next-lemma
	 ;;(format nil "~{<lemma wtag='~A' lem='~A' affix='~A' num='~A' wnum='~A' pos='~A'/>~}" (cadr lemma))
	 (+ 2 level))))
  ;;(format t "lemma-list:~A~%" *current-sentence*)
    (print-xml-tabs out-str "</lemma-list>" (1+ level)))
  )

(defun xml-end-sentence-print (out-str)  
  (print-xml-tabs out-str "</sentence>" 1 nil)
  (terpri out-str)
  (force-output out-str)
  )

(defun xml-xparse-print (out-str string)
  (print-xml-tabs out-str (format nil "<xparse><![CDATA[~A]]></xparse>" string) 3)
  )

(defun xml-start-tree (out-str level)
  (print-xml-tabs out-str "<tree>" level))

(defun xml-end-tree (out-str level)
  (print-xml-tabs out-str "</tree>" level))

(defun xml-start-node (out-str rule level)
  (print-xml-tabs out-str (format nil "<node rule='~A'>" rule) level))

(defun xml-end-node (out-str level)
  (print-xml-tabs out-str "</node>" level nil))

(defun xml-wnode (out-str wnode level)
  (print-xml-tabs out-str (format nil "<wnode lemma='~A'/>" wnode) level))

(defun xml-gr (out-str gr level)
  (let ((gr-string "<gr")
	(gr-tokens (mapcar #'xml-escaped-output  gr)))
    (unless (equal "" (car gr-tokens))
      (setq gr-string (format nil "~A type='~A'" gr-string (car gr-tokens))))
    (unless (equal "" (cadr gr-tokens))
      (setq gr-string (format nil "~A subtype='~A'" gr-string (cadr gr-tokens))))
    (unless (equal "" (caddr gr-tokens))
      (setq gr-string (format nil "~A head='~A'" gr-string (caddr gr-tokens))))
    (unless (equal "" (cadddr gr-tokens))
      (setq gr-string (format nil "~A dep='~A'" gr-string (cadddr gr-tokens))))
    (unless (equal "" (car (cddddr gr-tokens)))
      (setq gr-string (format nil "~A init='~A'" gr-string (car (cddddr gr-tokens)))))
    (setq gr-string (format nil "~A>" gr-string))
  (print-xml-tabs out-str 
		  gr-string
		  ;;(format nil "~{<gr type='~A' subtype='~A' head='~A' dep='~A' init='~A'>~}" (mapcar #'xml-escaped-output  gr))
		  level)))

(defun xml-end-gr (out-str level)
  (print-xml-tabs out-str "</gr>" level nil))

(defun xml-gr-weight (out-str gr-weight level)
  (if (= gr-weight 1.0)
      (print-xml-tabs out-str "<gr-weight weight='1.0'/>" level nil)
    (print-xml-tabs out-str (format nil "<gr-weight weight='~,6F'/>" gr-weight) level nil)))

(defun xml-gr-type (out-str gr-type level)
  (print-xml-tabs out-str (format nil "<gr-type>~A</gr-type>" gr-type) level))

(defun xml-gr-head (out-str gr-head level)
  (print-xml-tabs out-str (format nil "<gr-head>~A</gr-head>" gr-head) level))

(defun xml-gr-dep (out-str gr-dep level)
  (print-xml-tabs out-str (format nil "<gr-dep>~A</gr-dep>" gr-dep) level))

(defun xml-gr-init (out-str gr-init level)
  (print-xml-tabs out-str (format nil "<gr-init>~A</gr-init>" gr-init) level))

(defun xml-gr-subtype (out-str gr-subtype level)
  (print-xml-tabs out-str (format nil "<gr-subtype>~A</gr-subtype>" gr-subtype) level))

(defun xml-print-parse-set-start (out-str raw-trees idx)
    (when *first-tree-p*
      (format out-str "~&<nbest-parses num='~A'> ~%" (length raw-trees)))
    (format out-str "~&<parse-set pnum='~A' score='~,3F'> ~%" idx *tree-weight*))

(defun xml-print-parse-set-end (out-str)
    (format out-str "~&</parse-set>")
    (when *last-tree-p*
      (format out-str "~&</nbest-parses>")))

;; with -u and -s and -z output format ---------------------------
(defun lr1-parse-analysis-upenn-print-xml (tree out-str &optional (level 2))
  (xml-start-tree out-str (1+ level))
  (xml-tree-output out-str tree (+ 2 level) nil)
  (xml-end-tree out-str (1+ level)))
  
;; with -gio
(defun print-extracted-weighted-grs-xml (gr-sets out-str &optional (level 2))
  (if (consp (car gr-sets)) ;; frag parse:
      (lr1-parse-analysis-grs-print-with-weights-xml gr-sets out-str)
      (let ((grw-set (cadr gr-sets)))
	(setf *read-default-float-format* 'single-float) ;; get E instead of S!
	(dolist (grw grw-set)
	  (when (>= (cadr grw) +gr-threshold+)
	    (xml-gr-output out-str (car grw) (+ 3 level))
	    (when (not +parc700+)
	      (if (eql (cadr grw) 1.0s0)
		  (xml-gr-weight out-str 1.0 (+ 3 level))
		  (xml-gr-weight out-str (cadr grw) (+ 3 level))))
	    (xml-end-gr out-str (+ 2 level))
	    )
	  )
	(setf *read-default-float-format* 'double-float)
	)))

;; with -t output format ----------------------------------
;;(defun xml-tree-print (out-str tree level)
 ;; (xml-rawtree-output out-str tree level)
 ;; )

;; use with -t or -a or -u format
(defun print-tree-output-xml (tree out-str)
  ;;(format t "xml-t-print~%")
  (xml-start-tree out-str 3)
  (xml-tree-output out-str tree 4)
  (xml-end-tree out-str 3))

(defun xml-tree-output (out-str tree level &optional(assoc-p t))
  (when (not (atom tree))
    (xml-start-node out-str (car tree) level))
  (setq level (1+ level))
  (do* ((daughter-list (cdr tree) (cdr daughter-list)))
      ((null daughter-list))
    ;;(format t "daughter ~A ~%" (car daughter-list))
    (if (atom (car daughter-list))
	(let ((lemma 
	       (or (and assoc-p 
			(cadr (assoc (car daughter-list) ;;(intern (car daughter-list)) 
				     *current-sentence* :test #'equal)))
		   (split-word (car daughter-list)))))
	  ;;(format t "lemma for tree: ~A ~S~%" (car daughter-list) lemma)
	  ;;(format t "lemma for tree: ~A~%" lemma)
	  (xml-wnode out-str (cadddr lemma) level))
      (xml-tree-output out-str (car daughter-list) level)))
  (setq level (1- level))
  (when (not (atom tree))
    (xml-end-node out-str level)))

;; with -g output format ------------------------------------

(defun lr1-parse-analysis-grs-print-xml (grs-sets out-str &optional (level 2))
    (xml-gr-list-output out-str grs-sets (1+ level)))

;; use with -g and -tg
(defun xml-gr-list-output (out-str grs-sets level)
  (loop for grs in grs-sets
      do
	(dolist (gr grs)
	  (xml-gr-output out-str gr (+ 2 level))
	  (xml-end-gr out-str (1+ level))))
  )

;; with -gw output format ----------------------------------

(defun lr1-parse-analysis-grs-print-with-weights-xml (grs-sets out-str &optional (level 2))
   (declare (special +parseval-output-p+ *first-tree-p* *last-tree-p* *tree-weight*))
   (when *first-tree-p* (setq *weights-and-grs* nil))
   (dolist (grs grs-sets)
      (push (cons (- *tree-weight* (log (length grs-sets) 10)) grs)
         *weights-and-grs*))
   (when *last-tree-p*
      (let ((gr-table (make-hash-table :test #'equal))
            (total 0))
         (dolist (weight-and-grs *weights-and-grs*) ; in reverse rank order
            ;; compute in double precision otherwise underflow
            (let* ((wi (expt 10.0D0 (car weight-and-grs)))
                   (weight wi))
               (incf total weight)
               (dolist (gr (cdr weight-and-grs))
		 (incf (gethash gr gr-table 0) weight))))
         (maphash
            #'(lambda (gr w)
		(xml-gr-output out-str gr (+ 3 level))
		(when (not +parc700+)
		  (if (eql w total)
		      (xml-gr-weight out-str 1.0 (+ 3 level))
		    (xml-gr-weight out-str (coerce (/ w total) 'single-float) (+ 3 level))))
		;; get E rather than D		   
		(xml-end-gr out-str (+ 2 level)))
            gr-table)
         (setq *weights-and-grs* nil))))

;;----------------------------------------------------------------------------------
;; print out a gr:

(defun xml-gr-output (out-str gr level)
  (let ((gr-type (car gr))
	(gr-subtype "")
	(gr-head "")
	(gr-dep "")
	(gr-init "")
	(gr-items1 (cadddr (or 
			    (cadr (assoc (cadr gr);;(intern (cadr gr))
					 *current-sentence* :test #'equal))
		    (split-word (cadr gr)))))
	(gr-items2 (cadddr (or 
			    (cadr (assoc (caddr gr);;(intern (caddr gr))
					 *current-sentence* :test #'equal))
			    (split-word (caddr gr)))))
	(gr-items3 (cadddr (or 
			    (cadr (assoc (cadddr gr);;(intern (cadddr gr))
					 *current-sentence* :test #'equal))
			    (split-word (cadddr gr))))))
    ;;(xml-gr-type out-str gr-type level)
    ;;(pprint gr)
    ;;(pprint *current-sentence*)
    ;;(format t "~%1 ~S 2 ~S 3 ~S~%" gr-items1 gr-items2 gr-items3)
    (cond 
     ;; (type subtype head dependent)
     ((member gr-type '(|dependent| |mod| |ncmod| |xmod| |cmod| |arg_mod| |arg| |xcomp| |ccomp| |ta|))
      ;; subtype may now be e.g. that:4_CST - want the number then!
      ;;
      ;;(format t "~&gr-subtype:~A ~A~%" (cadr gr) (cadddr (cadr (assoc (cadr gr);;(intern (cadr gr)) *current-sentence* :test #'equal))))
      (setq gr-subtype (or
			;; its one of the words in the sentence e.g. that:4_CST:
			(cadddr (cadr (assoc (cadr gr);;(intern (cadr gr))
				     *current-sentence* :test #'equal)))
			;; or is 'that' 'bal' etc
			(cadr gr)))
      (setq gr-head gr-items2)
      (setq gr-dep gr-items3))
     ;; (head dependent)
     ((member gr-type 
	      '(|pmod| |det| |subj_dobj| |comp| |obj| |dobj| |obj2| |iobj| |clausal| |pcomp| |aux| |conj|))
      (setq gr-head gr-items1)
      (setq gr-dep gr-items2))
     ;; (head dependent initial_gr)
     ((member gr-type '(|subj| |ncsubj| |xsubj| |csubj|))
      (setq gr-head gr-items1)
      (setq gr-dep gr-items2)
      (setq gr-init gr-items3))
     ;; head
     ((member gr-type '(|passive|))
      (setq gr-head gr-items1)
      ))
    (xml-gr out-str (list gr-type gr-subtype gr-head gr-dep gr-init) level)
    ))

;;;(defun xml-gr-output (out-str gr level)
;;;  (let ((gr-type (car gr)))
;;;    (xml-gr-type out-str gr-type level)
;;;    (cond 
;;;     ;; (type head dependent)
;;;     ((member gr-type '(|dependent| |mod| |ncmod| |xmod| |cmod| |arg_mod| |arg| |xcomp| |ccomp| |ta|))
;;;      
;;;      
;;;      (xml-gr-subtype out-str (cadr gr) level)
;;;      (xml-gr-head out-str (cadddr (split-word (caddr gr))) level)
;;;      (xml-gr-dep out-str (cadddr (split-word (cadddr gr))) level))
;;;     ;; (head dependent)
;;;     ((member gr-type 
;;;	      '(|pmod| |det| |subj_dobj| |comp| |obj| |dobj| |obj2| |iobj| |clausal| |pcomp| |aux| |conj|))
;;;      (xml-gr-head out-str (cadddr (split-word (cadr gr))) level)
;;;      (xml-gr-dep out-str (cadddr (split-word (caddr gr))) level))
;;;     ;; (head dependent initial_gr)
;;;     ((member gr-type '(|subj| |ncsubj| |xsubj| |csubj|))
;;;      (xml-gr-head out-str (cadddr (split-word (cadr gr))) level)
;;;      (xml-gr-dep out-str (cadddr (split-word (caddr gr))) level)
;;;      (xml-gr-init out-str (cadddr (split-word (caddr gr))) level))
;;;     ;; head
;;;     ((member gr-type '(|passive|))
;;;      (xml-gr-head out-str (cadddr (split-word (cadr gr))) level))
;;;     )))

;;----------------------------------------------------------------------------------
;; general functions:  

;; return the (wtag) word (affix) number, pos
(defun split-word (orig)
  ;;format t "~%splitting:~A~%" orig)
  (when (not (atom orig))
    (return-from split-word (list nil nil 0 0 nil)))
  (let ((form (format nil "~A" orig))
	(wtag "") 
	(word "") 
	(affix "")
	(number "")
	(pos ""))
    (when (and (> (length form) 1)
	       (eql (char form 0) #\<) (eql (char form 1) #\w))
      (let*
	  ((wtag-end (position #\> form :start 2))
	   (endwtag-start
	    (and wtag-end (position #\< form :from-end t))))
	(when (and wtag-end endwtag-start)
	  ;(setq wtag (subseq form 0 (1+ wtag-end)))
	  (setq wtag (subseq form 3 wtag-end))
	  (setq form (subseq form (1+ wtag-end) endwtag-start)))))
    (let*
	((pos-p (position #\_ form :from-end t))
	 (affix-p (position #\+ form :from-end t :end pos-p))
	 (num-p (position #\: form :from-end t :end pos-p))
	 (epos (or affix-p num-p pos-p)))
      (if epos 
	  (if (= epos 0)
	      (setq word (subseq form 0 1))
	    (setq word (subseq form 0 epos)))
	(setq word form))
      ;;(format t "form:~A num-p ~A ~%" form num-p )
      (when affix-p (setq affix (subseq form (1+ affix-p) (or num-p pos-p))))
      (when num-p (setq number (subseq form (1+ num-p) pos-p)))
      (when pos-p (setq pos (subseq form (1+ pos-p))))
      ;;(format t "affix:~A num-p:~A pos-p:~A epos:~A ~%" affix-p num-p pos-p epos)
      (if epos
	  (list wtag word affix number number pos)
	  (let ((orig-str (string orig)))
	    (list orig-str orig-str orig-str orig-str orig-str orig-str))))))

;; end

;;;(defun escape-quotes (str)
;;;  ;;(format t "escap-quotes :~A~%" str)
;;;  (let ((new-str ""))
;;;    (dotimes (i (length str))
;;;      (let ((next-ch (elt str i)))
;;;	(setq new-str (concatenate 'string new-str
;;;				   (if (char= next-ch #\")
;;;				       "\"" (string next-ch))))))
;;;    ;;(format t "escap-quotes new :~A~%" new-str)
;;;    new-str))

;; this has been put in out-analysis.lsp instead - stops a weird embed
;; error as this function is embedded and within the function is referral
;; to other embedded functions below!
