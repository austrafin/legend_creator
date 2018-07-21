;; Automatic Legend Creator
;; Author Matti Syrjänen - Distribution Power Design
;; Version 1.0
;; Lee Mac's 'Import Block' version 1.2 used as a template

(defun c:CREATELEGEND ( / abc blk dbx doc dwg ins objects mapping_found mapblock)
(setq dwg ; User defines the string in 'Settings' section below









;************************************************************************************
;************************  ______________________________  **************************
;************************ |								 | **************************
;************************ |	  Automatic Legend Creator	 | **************************
;************************ |	  Distribution Power Design	 | **************************
;************************ |______________________________| **************************
;************************								   **************************
;************************************************************************************



;************************************************************************************
;****************************  ______________  **************************************
;**************************** |				 | **************************************
;**************************** |	  Settings	 | **************************************
;**************************** |______________| **************************************
;**************************** 				  ***************************************
;************************************************************************************

	; Write the address to the block library file below. For example:				
	; "C:\\XXX\\LEGEND BLOCKS.dwg"
	; Note that any slashes must be duplicated, for example,
	; "C:\XXX\LEGEND BLOCKS.dwg"
	; becomes:
	; "C:\\XXX\\LEGEND BLOCKS.dwg"
	"C:\\XXX\\LEGEND BLOCKS.dwg"
	
	
	)(setq offset ; <- Don't worry about this, it's required for the program to work :)
	
	
	; Alter this number to change the vertical distance between the legend blocks
	6
	
	)(setq legend_layer ; <- Again, ignore this
	
	; Specify the layer where the legend blocks are inserted to
	"0"
	
	)(setq mapping (list ; <- Again, ignore this
	
	
	; Write the block mappings below. To add a new entry, type:
	; (list "[block or layer name]" "[visibility state if applicable, otherwise nil]" "[name of the legend block]" [group id or nil])
	
	; just a few examples:
	(list "block_name_1" nil "legend_block_name_1" nil)
	(list "block_name_2" "visibility_state" "legend_block_name_2" nil)
	(list "block_name_3" "visibility_state_1" "legend_block_name_3" 0)
	(list "block_name_3" "visibility_state_1" "legend_block_name_3" 0)
	
	
;****************************************************************************************			
;****************************************************************************************	
;****************************************************************************************	
;****************************************************************************************	
	
	
	

	
	
	)) ; end of setq and mapping list

    (cond
        (   (= (strcase dwg) (strcase (vla-get-fullname (setq doc (vla-get-activedocument (vlax-get-acad-object))))))
            (princ "\nError: The block legend library file must be different to the current drawing.")
        )
        (   (not (setq dbx (LM:GetDocumentObject dwg)))
            (princ "\nError: Unable to interface with selected drawing.")
        )
		(	(not (tblsearch "LAYER" legend_layer))
			(princ (strcat "\nError: Layer \"" legend_layer "\" does not exist."))
		)

        (   t
			(if (setq selection_set (ssget))
				(progn
					(setq i 0
						  ss_size (sslength selection_set)
					)
					
					(while (< i ss_size)
						(setq e (ssname selection_set i)
							  i (1+ i)
						)
						(cond
							; If not a block
							((not (eq (cdr (assoc 0 (entget e))) "INSERT")) (setq objects (cons (list (cdr (assoc 8 (entget e))) nil) objects))) 
							
							(t ; If a block
								(setq visibilitystate (LM:getvisibilitystate (vlax-ename->vla-object e)))
								(setq objects 	(cons
													(list
														(LM:al-effectivename e) ; block name
														(LM:getvisibilitystate (vlax-ename->vla-object e)) ; visibility state
													 )
												objects)
								)
							)
						)
					)
					
					(setq
						ins (getpoint "\nSpecify the starting point for the legend: ")
						
						; Save the current layer so that we can revert back to it after the legend has been created
						current_layer (getvar "CLAYER")
					)
					(setvar "CLAYER" legend_layer)

					; Iterate through the selected selected objects and the legend blocks in the block library file
					
					(while (> (length mapping) 0)
						(setq
							m (car mapping)
							mapping (cdr mapping)
							mapping_found 0
							i 0
							objects_tail objects
						)
						
						(while (and (< i ss_size) (eq mapping_found 0))
							(setq
								; object is the iterator - take the head of the list and leave the tail for the next iteration
								object (car objects_tail)
								objects_tail (cdr objects_tail)
							)

							(if (and (eq (car object) (car m)) (eq (cadr object) (cadr m)))
								(progn
									(setq mapblock (caddr m))
									(setq mapping_found 1)
								)
							)
							(setq i (1+ i))
						)
						
						(if (and
								(eq 1 mapping_found)
								(not
									(vl-catch-all-error-p
										(vl-catch-all-apply 'vlax-invoke
											(list dbx 'copyobjects
												(list (LM:getitem (vla-get-blocks dbx) (setq blk mapblock)))
												(setq abc (vla-get-blocks doc))
											)
										)
									)
								)
								(LM:getitem abc blk)
							)
							(progn
								(vla-insertblock
									(vlax-get-property doc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
									(vlax-3D-point (trans ins 1 0))
									 blk
									 1.0 1.0 1.0
									(angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans '(0.0 0.0 1.0) 1 0 t) t))
								)
								
								
								(setq
									id (nth 3 m) ; unique identifier of a 'group'
								
									; Change the Y-coordinate of the insertion point so that the next legend block will be inserted below the previous one
									ins (list (car ins) (- (cadr ins) offset) (caddr ins))
								)
								
								; if the legend mapping is part of a specific 'group' of mappings, remove the list entries with the same id
								(if (not (eq id nil))
									(foreach m2 mapping
										(if (eq (nth 3 m2) id)
											(setq mapping (vl-remove m2 mapping))
										)
									)
								)
							)
						)
					)
					(setvar "CLAYER" current_layer)
				)
			)
        )
    )
    (princ)
)

;; Get Document Object  -  Lee Mac
;; Retrieves the VLA Document Object for the supplied filename.
;; The Document Object may be present in the Documents collection, or obtained through ObjectDBX.
;; It is the callers responsibility to release such object.

(defun LM:GetDocumentObject ( dwg / app dbx dwl vrs )
    (cond
        (   (not (setq dwg (findfile dwg))) nil)
        (   (cdr
                (assoc (strcase dwg)
                    (vlax-for doc (vla-get-documents (setq app (vlax-get-acad-object)))
                        (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                    )
                )
            )
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list app
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx dwg))))
            dbx
        )
    )
)

;; VLA-Collection: Get Item  -  Lee Mac
;; Retrieves the item with index 'idx' if present in the supplied collection
;; col - [vla]     VLA Collection Object
;; idx - [str/int] Index of the item to be retrieved
 
(defun LM:getitem ( col idx / obj )
    (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-item (list col idx)))))
        obj
    )
)

;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil
;; [Matti Syrjanen] Modified so that it first checks whether the LM:getvisibilityparametername returns nil

(defun LM:getvisibilitystate ( blk )
	(setq visibilityname (LM:getvisibilityparametername blk))
	(if (not (eq nil visibilityname)) (LM:getdynpropvalue blk visibilityname))    
)

;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)

(defun LM:getdynpropvalue ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)

;; Effective Block Name  -  Lee Mac
;; ent - [ent] Block Reference entity

(defun LM:al-effectivename ( ent / blk rep )
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("AcDbBlockRepBTag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)