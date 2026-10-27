(defun c:GPDIM
  (/ *error* doc spc
     p1 p2 ss
     basePts midPts allPts pts pts2
     xs ys xmin xmax ymin ymax
     bxmin bxmax bymin bymax
     dimloc dir i pA pB
     pFirst pLast totalPts totalDimLoc
     tol minlen gap cx cy oldosmode oldcmdecho)

  (vl-load-com)

  ;; -------------------------
  ;; Error handler
  ;; -------------------------

  (defun *error* (msg)
    (if oldosmode (setvar "OSMODE" oldosmode))
    (if oldcmdecho (setvar "CMDECHO" oldcmdecho))
    (if (and msg (/= msg "Function cancelled"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; -------------------------
  ;; Settings
  ;; -------------------------

  (setq tol 0.001)

  ;; Ignore dimensions shorter than this.
  ;; Set 0.0 to keep all.
  (setq minlen 0.0)

  ;; -------------------------
  ;; Point utilities
  ;; -------------------------

  (defun _valid-pt-p (p)
    (and
      (listp p)
      (numberp (car p))
      (numberp (cadr p))
    )
  )

  (defun _pt2d (p)
    (if (_valid-pt-p p)
      (list (car p) (cadr p) 0.0)
      nil
    )
  )

  (defun _dist (a b / pa pb)
    (setq pa (_pt2d a))
    (setq pb (_pt2d b))
    (if (and pa pb)
      (distance pa pb)
      999999999.0
    )
  )

  (defun _addpt (p lst / pt exist q)
    (setq pt (_pt2d p))

    (if pt
      (progn
        (setq exist nil)

        (foreach q lst
          (if (< (_dist pt q) tol)
            (setq exist T)
          )
        )

        (if exist
          lst
          (cons pt lst)
        )
      )
      lst
    )
  )

  (defun _appendpts (src dst / p)
    (foreach p src
      (setq dst (_addpt p dst))
    )
    dst
  )

  (defun _inside-box-p (p / pt)
    (setq pt (_pt2d p))

    (if pt
      (and
        (>= (car pt) (- bxmin tol))
        (<= (car pt) (+ bxmax tol))
        (>= (cadr pt) (- bymin tol))
        (<= (cadr pt) (+ bymax tol))
      )
      nil
    )
  )

  ;; -------------------------
  ;; Sort and unique
  ;; -------------------------

  (defun _sortpts-x (lst)
    (vl-sort
      lst
      '(lambda (a b)
         (if (< (abs (- (car a) (car b))) tol)
           (< (cadr a) (cadr b))
           (< (car a) (car b))
         )
       )
    )
  )

  (defun _sortpts-y (lst)
    (vl-sort
      lst
      '(lambda (a b)
         (if (< (abs (- (cadr a) (cadr b))) tol)
           (< (car a) (car b))
           (< (cadr a) (cadr b))
         )
       )
    )
  )

  (defun _unique-by-x (lst / result p q exists)
    (setq result nil)

    (foreach p (_sortpts-x lst)
      (setq exists nil)

      (foreach q result
        (if (< (abs (- (car p) (car q))) tol)
          (setq exists T)
        )
      )

      (if (not exists)
        (setq result (append result (list p)))
      )
    )

    result
  )

  (defun _unique-by-y (lst / result p q exists)
    (setq result nil)

    (foreach p (_sortpts-y lst)
      (setq exists nil)

      (foreach q result
        (if (< (abs (- (cadr p) (cadr q))) tol)
          (setq exists T)
        )
      )

      (if (not exists)
        (setq result (append result (list p)))
      )
    )

    result
  )

  ;; -------------------------
  ;; CANNOSCALE factor
  ;; gap = 10 * factor
  ;; Handles both 5 and 0.2 cases.
  ;; -------------------------

  (defun _get-cannoscale-factor (/ v s pos left right factor)
    (setq factor nil)

    ;; Prefer CANNOSCALE string, for example "1:5"
    (setq s (getvar "CANNOSCALE"))

    (if s
      (progn
        (setq pos (vl-string-search ":" s))

        (if pos
          (progn
            ;; vl-string-search is zero-based.
            ;; substr is one-based.
            (setq left  (atof (substr s 1 pos)))
            (setq right (atof (substr s (+ pos 2))))

            (if (and (> left 0.0) (> right 0.0))
              (setq factor (/ right left))
            )
          )
        )
      )
    )

    ;; Fallback to CANNOSCALEVALUE
    (if (not factor)
      (progn
        (setq v (getvar "CANNOSCALEVALUE"))

        (if (and v (numberp v) (> v 0.0))
          (progn
            ;; Some AutoCAD environments return 0.2 for 1:5.
            ;; Convert it back to 5.
            (if (< v 1.0)
              (setq factor (/ 1.0 v))
              (setq factor v)
            )
          )
          (setq factor 1.0)
        )
      )
    )

    factor
  )

  ;; -------------------------
  ;; Direction decision
  ;; Use selection window center, not object center.
  ;; Left / right of selection box -> Y dimension
  ;; Top / bottom of selection box -> X dimension
  ;; -------------------------

  (defun _decide-dir-by-dimloc (pt / dx dy)
    (setq cx (/ (+ bxmin bxmax) 2.0))
    (setq cy (/ (+ bymin bymax) 2.0))

    (setq dx (abs (- (car pt) cx)))
    (setq dy (abs (- (cadr pt) cy)))

    (if (> dx dy)
      "Y"
      "X"
    )
  )

  ;; -------------------------
  ;; Total dimension location
  ;; Uses selection window center too.
  ;; -------------------------

  (defun _total-dim-location (dir dimpt / result)
    (setq cx (/ (+ bxmin bxmax) 2.0))
    (setq cy (/ (+ bymin bymax) 2.0))

    (cond
      ((= dir "X")
       (if (>= (cadr dimpt) cy)
         (setq result (list (car dimpt) (+ (cadr dimpt) gap) 0.0))
         (setq result (list (car dimpt) (- (cadr dimpt) gap) 0.0))
       )
      )

      ((= dir "Y")
       (if (>= (car dimpt) cx)
         (setq result (list (+ (car dimpt) gap) (cadr dimpt) 0.0))
         (setq result (list (- (car dimpt) gap) (cadr dimpt) 0.0))
       )
      )
    )

    result
  )

  ;; -------------------------
  ;; Find real extreme points for total dimension
  ;; -------------------------

  (defun _extreme-pts-for-total (pts dir / xs ys mn mx p pMin pMax)
    (cond
      ((= dir "X")
       (setq xs nil)

       (foreach p pts
         (setq xs (cons (car p) xs))
       )

       (setq mn (apply 'min xs))
       (setq mx (apply 'max xs))

       (foreach p pts
         (if (< (abs (- (car p) mn)) tol)
           (setq pMin p)
         )
         (if (< (abs (- (car p) mx)) tol)
           (setq pMax p)
         )
       )
      )

      ((= dir "Y")
       (setq ys nil)

       (foreach p pts
         (setq ys (cons (cadr p) ys))
       )

       (setq mn (apply 'min ys))
       (setq mx (apply 'max ys))

       (foreach p pts
         (if (< (abs (- (cadr p) mn)) tol)
           (setq pMin p)
         )
         (if (< (abs (- (cadr p) mx)) tol)
           (setq pMax p)
         )
       )
      )
    )

    (list pMin pMax)
  )

  ;; -------------------------
  ;; Curve middle grip candidates
  ;; -------------------------

  (defun _curve-mid-grips (ename / pts sp ep i mp)
    (setq pts nil)

    (setq sp
      (vl-catch-all-apply
        'vlax-curve-getStartParam
        (list ename)
      )
    )

    (setq ep
      (vl-catch-all-apply
        'vlax-curve-getEndParam
        (list ename)
      )
    )

    (if
      (and
        (not (vl-catch-all-error-p sp))
        (not (vl-catch-all-error-p ep))
      )
      (progn
        (setq i (fix sp))

        ;; Polyline segment middle is usually N + 0.5
        (while (< i ep)
          (setq mp
            (vl-catch-all-apply
              'vlax-curve-getPointAtParam
              (list ename (+ i 0.5))
            )
          )

          (if (not (vl-catch-all-error-p mp))
            (setq pts (_addpt mp pts))
          )

          (setq i (1+ i))
        )
      )
    )

    pts
  )

  ;; -------------------------
  ;; Add only edge middle grips
  ;; X dimension: add only minX / maxX middle grips
  ;; Y dimension: add only minY / maxY middle grips
  ;; -------------------------

  (defun _add-edge-middle-grips (base mids dir / pool xs ys mn mx p result)
    (setq result base)
    (setq pool nil)

    ;; Use base + mids to find true extremes.
    (setq pool (_appendpts base pool))
    (setq pool (_appendpts mids pool))

    (if (> (length pool) 0)
      (cond
        ((= dir "X")
         (setq xs nil)

         (foreach p pool
           (setq xs (cons (car p) xs))
         )

         (setq mn (apply 'min xs))
         (setq mx (apply 'max xs))

         (foreach p mids
           (if
             (or
               (< (abs (- (car p) mn)) tol)
               (< (abs (- (car p) mx)) tol)
             )
             (setq result (_addpt p result))
           )
         )
        )

        ((= dir "Y")
         (setq ys nil)

         (foreach p pool
           (setq ys (cons (cadr p) ys))
         )

         (setq mn (apply 'min ys))
         (setq mx (apply 'max ys))

         (foreach p mids
           (if
             (or
               (< (abs (- (cadr p) mn)) tol)
               (< (abs (- (cadr p) mx)) tol)
             )
             (setq result (_addpt p result))
           )
         )
        )
      )
    )

    result
  )

  ;; -------------------------
  ;; LWPOLYLINE vertices
  ;; -------------------------

  (defun _lwpoly-base-grips (ename / data pts d pt)
    (setq data (entget ename))
    (setq pts nil)

    (foreach d data
      (if (= (car d) 10)
        (progn
          (setq pt (_pt2d (cdr d)))
          (if pt
            (setq pts (_addpt pt pts))
          )
        )
      )
    )

    pts
  )

  ;; -------------------------
  ;; Old POLYLINE vertices
  ;; -------------------------

  (defun _polyline-base-grips (ename / pts e data typ pt)
    (setq pts nil)
    (setq e (entnext ename))

    (while e
      (setq data (entget e))
      (setq typ (cdr (assoc 0 data)))

      (cond
        ((= typ "VERTEX")
         (setq pt (_pt2d (cdr (assoc 10 data))))
         (if pt
           (setq pts (_addpt pt pts))
         )
        )

        ((= typ "SEQEND")
         (setq e nil)
        )
      )

      (if e
        (setq e (entnext e))
      )
    )

    pts
  )

  ;; -------------------------
  ;; Base grip points
  ;; Endpoints / vertices / quadrant points
  ;; -------------------------

  (defun _get-base-grip-points (ename / data typ pts cen rad p10 p11 sp ep)
    (setq data (entget ename))
    (setq typ  (cdr (assoc 0 data)))
    (setq pts nil)

    (cond
      ((= typ "LINE")
       (setq p10 (_pt2d (cdr (assoc 10 data))))
       (setq p11 (_pt2d (cdr (assoc 11 data))))

       (setq pts (_addpt p10 pts))
       (setq pts (_addpt p11 pts))
      )

      ((= typ "LWPOLYLINE")
       (setq pts (_lwpoly-base-grips ename))
      )

      ((= typ "POLYLINE")
       (setq pts (_polyline-base-grips ename))
      )

      ((= typ "CIRCLE")
       (setq cen (_pt2d (cdr (assoc 10 data))))
       (setq rad (cdr (assoc 40 data)))

       (if cen
         (progn
           (setq pts (_addpt cen pts))
           (setq pts (_addpt (list (+ (car cen) rad) (cadr cen) 0.0) pts))
           (setq pts (_addpt (list (- (car cen) rad) (cadr cen) 0.0) pts))
           (setq pts (_addpt (list (car cen) (+ (cadr cen) rad) 0.0) pts))
           (setq pts (_addpt (list (car cen) (- (cadr cen) rad) 0.0) pts))
         )
       )
      )

      ((= typ "ARC")
       (setq cen (_pt2d (cdr (assoc 10 data))))

       (if cen
         (setq pts (_addpt cen pts))
       )

       (setq sp
         (vl-catch-all-apply
           'vlax-curve-getStartPoint
           (list ename)
         )
       )

       (setq ep
         (vl-catch-all-apply
           'vlax-curve-getEndPoint
           (list ename)
         )
       )

       (if (not (vl-catch-all-error-p sp))
         (setq pts (_addpt sp pts))
       )

       (if (not (vl-catch-all-error-p ep))
         (setq pts (_addpt ep pts))
       )
      )

      ((member typ '("INSERT" "TEXT" "MTEXT"))
       (setq pts (_addpt (cdr (assoc 10 data)) pts))
      )

      (T nil)
    )

    pts
  )

  ;; -------------------------
  ;; Middle grip candidates
  ;; They are only candidates.
  ;; Only edge middle grips will be added later.
  ;; -------------------------

  (defun _get-middle-grip-points (ename / data typ pts p10 p11 mid sp ep)
    (setq data (entget ename))
    (setq typ  (cdr (assoc 0 data)))
    (setq pts nil)

    (cond
      ((= typ "LINE")
       (setq p10 (_pt2d (cdr (assoc 10 data))))
       (setq p11 (_pt2d (cdr (assoc 11 data))))

       (if (and p10 p11)
         (setq pts
           (_addpt
             (list
               (/ (+ (car p10) (car p11)) 2.0)
               (/ (+ (cadr p10) (cadr p11)) 2.0)
               0.0
             )
             pts
           )
         )
       )
      )

      ((member typ '("LWPOLYLINE" "POLYLINE"))
       (setq pts (_curve-mid-grips ename))
      )

      ((= typ "ARC")
       (setq sp
         (vl-catch-all-apply
           'vlax-curve-getStartParam
           (list ename)
         )
       )

       (setq ep
         (vl-catch-all-apply
           'vlax-curve-getEndParam
           (list ename)
         )
       )

       (if
         (and
           (not (vl-catch-all-error-p sp))
           (not (vl-catch-all-error-p ep))
         )
         (progn
           (setq mid
             (vl-catch-all-apply
               'vlax-curve-getPointAtParam
               (list ename (/ (+ sp ep) 2.0))
             )
           )

           (if (not (vl-catch-all-error-p mid))
             (setq pts (_addpt mid pts))
           )
         )
       )
      )

      (T nil)
    )

    pts
  )

  ;; -------------------------
  ;; Get current space
  ;; -------------------------

  (defun _get-space (/ doc)
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

    (if (= 1 (getvar "CVPORT"))
      (vla-get-PaperSpace doc)
      (vla-get-ModelSpace doc)
    )
  )

  ;; -------------------------
  ;; Create dimension directly by ActiveX
  ;; dir = "X" or "Y"
  ;; -------------------------

  (defun _add-linear-dim (pa pb dimpt dir / spc rot)
    (if
      (and
        (_valid-pt-p pa)
        (_valid-pt-p pb)
        (_valid-pt-p dimpt)
      )
      (progn
        (setq spc (_get-space))
        (setq rot
          (if (= dir "X")
            0.0
            (/ pi 2.0)
          )
        )

        (vla-AddDimRotated
          spc
          (vlax-3d-point (_pt2d pa))
          (vlax-3d-point (_pt2d pb))
          (vlax-3d-point (_pt2d dimpt))
          rot
        )
      )
    )
  )

  ;; -------------------------
  ;; Main
  ;; -------------------------

  (setq oldosmode (getvar "OSMODE"))
  (setq oldcmdecho (getvar "CMDECHO"))

  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  ;; gap = 10 * current annotation scale factor
  (setq gap (* 10.0 (_get-cannoscale-factor)))

  (setq p1 (getpoint "\nSpecify first corner of selection window: "))

  (if p1
    (progn
      (setq p2 (getcorner p1 "\nSpecify opposite corner: "))

      (if p2
        (progn
          (setq p1 (_pt2d p1))
          (setq p2 (_pt2d p2))

          (if (and p1 p2)
            (progn
              ;; Selection window range
              (setq bxmin (min (car p1) (car p2)))
              (setq bxmax (max (car p1) (car p2)))
              (setq bymin (min (cadr p1) (cadr p2)))
              (setq bymax (max (cadr p1) (cadr p2)))

              ;; Selection window center
              (setq cx (/ (+ bxmin bxmax) 2.0))
              (setq cy (/ (+ bymin bymax) 2.0))

              ;; Crossing selection
              (setq ss (ssget "_C" p1 p2))

              (if ss
                (progn
                  (setq basePts nil)
                  (setq midPts nil)
                  (setq i 0)

                  ;; Collect base grips and middle grip candidates inside selection window
                  (while (< i (sslength ss))
                    (foreach p (_get-base-grip-points (ssname ss i))
                      (if (_inside-box-p p)
                        (setq basePts (_addpt p basePts))
                      )
                    )

                    (foreach p (_get-middle-grip-points (ssname ss i))
                      (if (_inside-box-p p)
                        (setq midPts (_addpt p midPts))
                      )
                    )

                    (setq i (1+ i))
                  )

                  ;; Use base + middle candidates to calculate total range,
                  ;; because the outermost point may be a middle grip.
                  (setq allPts nil)
                  (setq allPts (_appendpts basePts allPts))
                  (setq allPts (_appendpts midPts allPts))

                  (if (> (length allPts) 1)
                    (progn
                      ;; Grip range
                      (setq xs nil)
                      (setq ys nil)

                      (foreach p allPts
                        (setq xs (cons (car p) xs))
                        (setq ys (cons (cadr p) ys))
                      )

                      (setq xmin (apply 'min xs))
                      (setq xmax (apply 'max xs))
                      (setq ymin (apply 'min ys))
                      (setq ymax (apply 'max ys))

                      ;; Pick inner dimension line location
                      (setq dimloc (getpoint "\nSpecify dimension line location: "))
                      (setq dimloc (_pt2d dimloc))

                      (if dimloc
                        (progn
                          ;; Decide direction by selection window center
                          (setq dir (_decide-dir-by-dimloc dimloc))

                          ;; Add only edge middle grips
                          (setq pts (_add-edge-middle-grips basePts midPts dir))

                          ;; X direction
                          (if (= dir "X")
                            (progn
                              (setq pts2 (_unique-by-x pts))
                              (setq pts2 (_sortpts-x pts2))

                              ;; Inner segment dimensions
                              (setq i 0)

                              (while (< i (1- (length pts2)))
                                (setq pA (nth i pts2))
                                (setq pB (nth (1+ i) pts2))

                                (if (>= (abs (- (car pB) (car pA))) minlen)
                                  (_add-linear-dim pA pB dimloc "X")
                                )

                                (setq i (1+ i))
                              )

                              ;; Outer total dimension
                              (if (> (length pts2) 2)
                                (progn
                                  (setq totalPts (_extreme-pts-for-total pts "X"))
                                  (setq pFirst (car totalPts))
                                  (setq pLast  (cadr totalPts))
                                  (setq totalDimLoc (_total-dim-location "X" dimloc))

                                  (if (and pFirst pLast totalDimLoc)
                                    (_add-linear-dim pFirst pLast totalDimLoc "X")
                                  )
                                )
                              )
                            )
                          )

                          ;; Y direction
                          (if (= dir "Y")
                            (progn
                              (setq pts2 (_unique-by-y pts))
                              (setq pts2 (_sortpts-y pts2))

                              ;; Inner segment dimensions
                              (setq i 0)

                              (while (< i (1- (length pts2)))
                                (setq pA (nth i pts2))
                                (setq pB (nth (1+ i) pts2))

                                (if (>= (abs (- (cadr pB) (cadr pA))) minlen)
                                  (_add-linear-dim pA pB dimloc "Y")
                                )

                                (setq i (1+ i))
                              )

                              ;; Outer total dimension
                              (if (> (length pts2) 2)
                                (progn
                                  (setq totalPts (_extreme-pts-for-total pts "Y"))
                                  (setq pFirst (car totalPts))
                                  (setq pLast  (cadr totalPts))
                                  (setq totalDimLoc (_total-dim-location "Y" dimloc))

                                  (if (and pFirst pLast totalDimLoc)
                                    (_add-linear-dim pFirst pLast totalDimLoc "Y")
                                  )
                                )
                              )
                            )
                          )

                          (princ
                            (strcat
                              "\nDone. Direction = "
                              dir
                              ", gap = "
                              (rtos gap 2 3)
                            )
                          )
                        )
                        (princ "\nCancelled dimension line location.")
                      )
                    )
                    (princ "\nNot enough grips inside selection window.")
                  )
                )
                (princ "\nNo objects selected.")
              )
            )
          )
        )
      )
    )
  )

  (setvar "OSMODE" oldosmode)
  (setvar "CMDECHO" oldcmdecho)

  (princ)
)