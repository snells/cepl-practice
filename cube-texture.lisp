;;;; gl-test.lisp

(in-package :cepl)


;;; for faster math
(defmacro def-constants ()
  `(progn (defconstant +vec-up+ (v! 0 1 0))
	  (defconstant +v31+ (v! 1 1 1))
	  (defconstant pi1 3.14159)
	  (defconstant pi3/4 (* (/ 3 4) 3.14159))
	  (defconstant pi2 (* pi1 2))
	  (defconstant pi/2 (/ pi1 2))))
(if (null (boundp '+vec-up+))
    (def-constants))

(defvar *texture* (devil-helper:load-image-to-texture "/ss/dl/col.png"))

;(setf *texture* (devil-helper:load-image-to-texture "/ss/prog/lisp/gl-test/rejoice.jpg"))

(defparameter *camera* nil)
(defparameter *camera-speed* 0.1)

(defvar update-mode 'rot)
(defvar update-x nil)
(defvar update-y nil)
(defvar update-z nil)
(defvar update-factor 0.0)

(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec3 :accessor col))

(defstruct-g pos-uv ()
  (position :vec3 :accessor pos)
  (uv-coord :vec2 :accessor uv))


(defun-g vert-basic ((vert pos-col) &uniform (cam camera) (model-to-world :mat4) (color :vec3))
  (values (* (cam->clip cam)
	     (* (world->cam cam)
		(* model-to-world
		   (v! (pos vert) 1.0))))
	  (v! (* color (col vert)) 1.0)))

;(defun-g vert-texture ((vert pos-uv) &uniform (cam camera) (model-to-world :mat4)) ;(transperancy :vec3))
;  (values (* (cam->clip cam)
; 	     (* (world->cam cam)
; 		(* model-to-world
; 		   (v! (pos vert) 1.0))))
; 	  (uv vert)))
;(defun-g frag-texture ((tex-coord :vec2) &uniform (tex :sampler-2d))
;  ;(v! (s~ (texture tex (* tex-coord 1)) :xyz))) ;1.0))
; 					;(s~ (texture tex (* tex-coord 1)) :xyz))
;  (texture tex tex-coord))

(defun-g vert-texture ((vert g-pnt) &uniform (cam camera) (model-to-world :mat4))
  (values (* (cam->clip cam)
 	     (* (world->cam cam)
 		(* model-to-world
 		   (v! (pos vert) 1.0))))
	  (norm vert)
	  (tex vert)))
(defun-g frag-texture ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(defun-g frag-basic ((color :vec4))
  color)


(defpipeline prog-basic ()
    (g-> #'vert-basic #'frag-basic))

(defpipeline prog-texture ()
    (g-> #'vert-texture #'frag-texture))

(defclass gl-object ()
  ((stream :initform nil :initarg :str :accessor str)
   (position :initform (v! 0 0 0) :initarg :pos :accessor pos
	     :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot
	     :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale
	  :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   ;(mesh :initarg :mesh :accessor mesh)
   (color :initform (v! 1 1 1) :initarg :col :accessor col
	  :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))))


(defmethod render ((obj gl-object))
  (let ((m2w (reduce #'m4:m* (list (m4:translation (pos obj))
				   (m4:rotation-from-euler (rot obj))
				   (m4:scale (scale obj))))))
    (prog-basic (str obj) :model-to-world m2w :color (col obj))))


(defmethod move ((obj gl-object) (vec simple-array))
  (incf (aref (pos obj) 0) (aref vec 0))
  (incf (aref (pos obj) 1) (aref vec 1))
  (incf (aref (pos obj) 2) (aref vec 2)))


(defmethod move ((obj gl-object) (vec cons))
  (let ((x 0))
    (dolist (i vec)
      (incf (aref (pos obj) x) i)
      (incf x))))

(defclass cube-base (gl-object) ())

(defclass cube-texture (cube-base)
  ((texture :initarg :texture :initform nil :accessor tex)))

(defmethod render ((cube cube-texture))
  (let ((m2w (reduce #'m4:m* (list (m4:translation (pos cube))
				   (m4:rotation-from-euler (rot cube))
				   (m4:scale (scale cube))))))
    (prog-texture (str cube) :model-to-world m2w :tex (tex cube))))
    





(defclass light ()
  ((position :initform (v! 0 10 0) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))



(let ((objects '()))
  (defun objects-add (key obj)
    (push obj objects)
    (push key objects))
  (defun objects-get (key)
    (getf objects key))
  (defun objects-empty ()
    (setf objects '()))
  (defun objects-apply-all (fn)
    (let ((x 0))
      (dolist (o objects)
	(if (oddp x)
	    (funcall fn o))
	(incf x)))))

(defun make-cube-basic (pos)
  (let* ((array (make-gpu-array (list (list (v! -0.5 -0.5 0.5)  (v! 0.0 1.0 0.0))     
				     (list (v! 0.5 -0.5 0.5)   (v! 0.0 1.0 0.0))     
				     (list (v! 0.5 0.5 0.5)    (v! 0.0 0.0 1.0))     
				     (list (v! -0.5 0.5 0.5)   (v! 0.0 0.0 1.0))     
				     
				     (list (v! -0.5 -0.5 -0.5)  (v! 0.0 1.0 0.0))    
				     (list (v! 0.5 -0.5  -0.5)  (v! 0.0 1.0 0.0))    
				     (list (v! 0.5 0.5   -0.5)  (v! 0.0 0.0 1.0))    
				     (list (v! -0.5 0.5  -0.5)  (v! 0.0 0.0 1.0)))
			       :element-type 'pos-col :dimensions 8))
	 (ind (make-gpu-array '(0 1 2   0 2 3 ;front
				4 5 6   4 6 7 ;back
				0 1 4   1 4 5 ;bottom
				3 2 6   3 6 7 ;roof
				0 3 4   3 4 7 ;left side
				1 2 5   2 5 6 ;right side
				)
			     :element-type :unsigned-short :dimensions 36))
	 (estr (make-buffer-stream array :index-array ind)))
    (make-instance 'gl-object :str estr :pos pos)))

(let* ((box-data (primitives:box-data))
       (d (car box-data))
       (i (cadr box-data))
       (data (make-gpu-array d :element-type 'g-pnt))
       (ind (make-gpu-array i :element-type :ushort))
       (str (make-buffer-stream data :index-array ind)))
  (defun make-cube-texture (pos tex)
    (make-instance 'cube-texture :str str :pos pos :texture tex)))


(defun init ()
  (gl:disable :cull-face)  
  (setf *camera* (make-camera))
  ;(setf (pos *camera*) (v! 0 0 0))
  (setf (pos *camera*) (v! -0.60561156 24.303896 17.88054)
	(dir *camera*) (v! 0.7567606 -0.81387806 -0.6536922))
  (prog-texture nil :cam *camera*)
  (objects-empty)
  ;(objects-add :cube (make-cube-basic (v! 0 0 0)))
  (dotimes (x 10)
    (dotimes (y 10)
      (dotimes (z 10)
   	(objects-add :cube (make-cube-texture (v! x y z) *texture*))))))
					;(make-cube-basic (v! x y z)))))))


(defmethod update ((obj gl-object))
  (let* ((r (rot obj))
	 (rx (aref r 0))
	 (ry (aref r 1))
	 (rz (aref r 2))
	 (p (pos obj))
	 (px (aref p 0))
	 (py (aref p 1))
	 (pz (aref p 2))
	 (s (scale obj))
	 (sx (aref s 0))
	 (sy (aref s 1))
	 (sz (aref s 2)))
    (setf (pos obj) (update-pos px py pz))
    (setf (rot obj) (update-rot rx ry rz))
    (setf (scale obj) (update-scale sx sy sz))))

(defun update-pos (x y z)
  (if (eq update-mode 'move)
      (v! (if update-x (+ x update-factor) x)
	  (if update-y (+ y update-factor) y)
	  (if update-z (+ z update-factor) z))
      (v! x y z)))

(defun update-rot (x y z)
  (if (eq update-mode 'rot)
      (v! (if update-x (mod (+ x update-factor) pi2) x)
	  (if update-y (mod (+ y update-factor) pi2) y)
	  (if update-z (mod (+ z update-factor) pi2) z))
      (v! x y z)))

(defun update-scale (x y z)
    (if (eq update-mode 'scale)
      (v! (if update-x (+ x update-factor) x)
	  (if update-y (+ y update-factor) y)
	  (if update-z (+ z update-factor) z))
      (v! x y z)))


  
(defun reshape (dimensions)
  (setf (frame-size *camera*) dimensions)
  (prog-basic nil :cam *camera*))

(defun main-loop ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-texture nil :cam *camera*)
  (objects-apply-all #'render)
  (objects-apply-all #'update)
  (update-display))


(let ((end nil))
  (defun main ()
    (setf end nil)
    (init)
    (reshape (current-viewport))
    (loop :while (not end) :do (continuable (main-loop))))
  
  (defun stop ()
    (setf end t)))

(evt:def-event-listener sys-listener (e :sys)
  (when (typep e 'evt:will-quit) (stop)))

(evt:def-event-listener window-listener (e :window)
  (when (eq (evt:action e) :resized)
    (reshape (evt:data e))))



(defun right-vector (vec)
  (vector3:cross vec +vec-up+))
(let ((wire-frame nil))
  (evt:def-event-listener keyboard-listener (e :keyboard)
    (when (eq (evt:key-state :space) :down)
      (setf (pos *camera*) (vector3:+ (pos *camera*) (v! 0 1 0))))
    (when (eq (evt:key-state :w) :down)
      (setf (pos *camera*) (vector3:+ (pos *camera*) (vector3:* (dir *camera*) *camera-speed*))))
    (when (eq (evt:key-state :s) :down)
      (setf (pos *camera*) (vector3:- (pos *camera*) (vector3:* (dir *camera*) *camera-speed*))))
    (let ((right-vec (right-vector (dir *camera*))))
      (when (eq (evt:key-state :a) :down)
	(setf (pos *camera*) (vector3:- (pos *camera*) (vector3:* right-vec *camera-speed*))))
      (when (eq (evt:key-state :d) :down)
	(setf (pos *camera*) (vector3:+ (pos *camera*) (vector3:* right-vec *camera-speed*)))))

    (when (eq (evt:key-state :z) :down)
      (setf update-mode 'rot))
    (when (eq (evt:key-state :x) :down)
      (setf update-mode 'move))
    (when (eq (evt:key-state :c) :down)
      (setf update-mode 'scale))

    (when (eq (evt:key-state :v) :down)
      (setf update-x (not update-x)))
    (when (eq (evt:key-state :b) :down)
      (setf update-y (not update-y)))
    (when (eq (evt:key-state :n) :down)
      (setf update-z (not update-z)))

    (when (eq (evt:key-state :o) :down)
      (incf update-factor 0.05))
    (when (eq (evt:key-state :p) :down)
      (decf update-factor 0.05))
    
    (when (eq (evt:key-state :1) :down)
      (setf wire-frame (not wire-frame))
      (if wire-frame
	  (gl:polygon-mode :front-and-back :line)
	  (gl:polygon-mode :front-and-back :fill)))))
    

(defun my-clamp (val min max)
  (cond ((< val min) min)
 	((> val max) max)
 	(t val)))

(defvar mouse-ang (v! 0 0))
(evt:def-event-listener mouse-listener (e :mouse)
  (when (typep e 'evt:mouse-motion)
    (let* ((d (evt:delta e))
	   (x (+ (/ (v:x d) -100.0) (v:x mouse-ang)))
	   (y (+ (/ (v:y d) -100.0) (v:y mouse-ang))))
      (setf y (my-clamp y (- pi/2) pi/2))
      (setf mouse-ang (v! x y)
	    (dir *camera*) (v! (sin (v:x mouse-ang))
			       (sin (v:y mouse-ang))
			       (cos (v:x mouse-ang)))))))

