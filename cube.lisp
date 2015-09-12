;;;; gl-test.lisp

(in-package :cepl)

(defmacro def-constants ()
  (if (null (boundp '+vector-up+))
      `(progn (defconstant +vector-up+ (v! 0 1 0))
	      (defconstant +v31+ (v! 1 1 1)))))
(def-constants)

(defparameter *camera* nil)
(defparameter *camera-speed* 0.1)


(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec3 :accessor col))

(defun-g vert-basic ((vert pos-col) &uniform (cam camera) (model-to-world :mat4) (color :vec3))
  (values (* (cam->clip cam)
	     (* (world->cam cam)
		(* model-to-world
		   (v! (pos vert) 1.0))))
	  (v! (* color (col vert)) 1.0)))

(defun-g frag-basic ((color :vec4))
  color)

(defpipeline prog-basic ()
    (g-> #'vert-basic #'frag-basic))


;;; if you define :type in defclass will it be more optimized or does it perform checking runtime?
(defclass gl-object ()
  ((stream :initform nil :initarg :str :accessor str)
   (position :initform (v! 0 0 0) :initarg :pos :accessor pos
	     :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot
	     :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale
	  :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
   (color :initform (v! 1 1 1) :initarg :col :accessor col
	  :type '(SIMPLE-ARRAY SINGLE-FLOAT (3)))))


(defmethod render ((obj gl-object))
  (let ((m2w (reduce #'m4:m* (list (m4:translation (pos obj))
				   (m4:rotation-from-euler (rot obj))
				   (m4:scale (scale obj))))))
    (prog-basic (str obj) :model-to-world m2w :color (col obj))))


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


;;; cube with side length 1, bottom color green, roof color blue
(defun make-cube ()
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
    (make-instance 'gl-object :str estr)))
		   
(defun init ()
  ; by default cull-face is enabled and (gl:cull-face :back)
  ; means that opengl won't draw backside of the object
  ; So I disable culling 
  (gl:disable :cull-face)
  (setf *camera* (make-camera))
  (setf (pos *camera*) (v! 0 0 0))
  (prog-basic nil :cam *camera*)
  (objects-empty)
  (objects-add :cube (make-cube)))

(defun reshape (dimensions)
  (setf (frame-size *camera*) dimensions)
  (prog-basic nil :cam *camera*))

(defun main-loop ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (prog-basic nil :cam *camera*)
  (objects-apply-all #'render)
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
  (vector3:cross vec +vector-up+))

(evt:def-event-listener keyboard-listener (e :keyboard)
  (when (eq (evt:key-state :w) :down)
    (setf (pos *camera*) (vector3:+ (pos *camera*) (vector3:* (dir *camera*) *camera-speed*))))
  (when (eq (evt:key-state :s) :down)
    (setf (pos *camera*) (vector3:- (pos *camera*) (vector3:* (dir *camera*) *camera-speed*))))
  (let ((right-vec (right-vector (dir *camera*))))
    (when (eq (evt:key-state :a) :down)
      (setf (pos *camera*) (vector3:- (pos *camera*) (vector3:* right-vec *camera-speed*))))
    (when (eq (evt:key-state :d) :down)
      (setf (pos *camera*) (vector3:+ (pos *camera*) (vector3:* right-vec *camera-speed*))))))
    

(defvar mouse-ang (v! 0 0))
(evt:def-event-listener mouse-listener (e :mouse)
  (when (typep e 'evt:mouse-motion)
    (let ((d (evt:delta e)))
      (setf mouse-ang (v2:+ (v! (/ (v:x d) -100.0)
                                (/ (v:y d) -100.0))
                            mouse-ang)
            (dir *camera*) (v! (sin (v:x mouse-ang))
                          (sin (v:y mouse-ang))
                          (cos (v:x mouse-ang)))))))

