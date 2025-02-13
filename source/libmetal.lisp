;;;; Codes are taken from Caten (https://github.com/hikettei/Caten/blob/main/source/byoc/metal.lisp)
(defpackage :libmetal
  (:use :cl :cffi :float-features)
  (:export #:mcompile #:defmkernel))
(in-package :libmetal)
;; ~~ CFFI Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defconstant +request-type-compile+ 13)

(defun ensure-foreign-library ()
  (load-foreign-library "/usr/lib/libobjc.dylib")
  (load-foreign-library "/System/Library/Frameworks/Metal.framework/Metal")
  (load-foreign-library "/System/Library/PrivateFrameworks/MTLCompiler.framework/MTLCompiler")
  (load-foreign-library "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
  (load-foreign-library "/usr/lib/libSystem.dylib"))

(ensure-foreign-library)

(defcfun "MTLCreateSystemDefaultDevice" :pointer)
(defcfun "sel_registerName" :pointer (name :pointer))
(defcfun "dispatch_data_create" :pointer (data :pointer) (offset :size) (x :pointer) (y :pointer))
(defcfun "objc_getClass" :pointer (name :string))
(defcfun "MTLCodeGenServiceCreate" :pointer (service-name :string))
(defcfun "MTLCodeGenServiceBuildRequest" :void (cgs :pointer) (unused :pointer) (request-type :int) (request :pointer) (request-len :size) (callback :pointer))
(defcfun "make_callback_closure" :pointer (callback :pointer))
(defcfun "free_callback_closure" :pointer (callback :pointer))

(defun sel (name) (with-foreign-string (*name name) (sel-registername *name)))
(defmacro msg (ptr selector restype &rest args)
  `(foreign-funcall "objc_msgSend" :pointer ,ptr :pointer (sel ,selector) ,@args ,restype))
(defun to-ns-str (str) (with-foreign-string (*str str) (msg (objc-getclass "NSString") "stringWithUTF8String:" :pointer :pointer *str)))
;; ~~ MTLCompiler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] METAL Parallel Compilation
;; CFFI assumes `defcallback` is placed in the toplevel of the file.
;; i.e.: (callback callback) will not create a new closure which is required to work MTLCompiler in parallel.
;; (If we only targeting sbcl) defcallback is just an wrapper of sb-alien:alien-lambda (https://koji-kojiro.github.io/sb-docs/build/html/sb-alien/macro/ALIEN-LAMBDA.html)
;; - 1. Use sb-alien:alien-lambda directly to create a new closure. (keep defcallback for ccl-bin/etc, etc)
;; - 2. If running on SBCL, BACKEND=METAL and PARALLEL>1 is available by using (1.) otherwise produce an error.
(defvar *callback-handler*)
(defcallback callback :void
    ((blockptr :pointer) (error :int32) (data :pointer) (datalen :size) (errormsg :pointer))
  (declare (ignore blockptr))
  (assert (eql :ready *callback-handler*) () "*call-back-handler* is not set to :ready.")
  (case error
    (0
     ;; offset from beginning to data = header size + warning size
     (let* ((octets (loop for i upfrom 0 below datalen collect (mem-aref data :uint8 i))))
       (multiple-value-bind (header warn)
           (cl-pack:unpack "<LL" (with-output-to-string (out) (map 'list #'(lambda (x) (princ (code-char x) out)) (subseq octets 8 16))))
         (when (not (= warn 0))
           (warn "Metal: ~a" (flexi-streams:octets-to-string (coerce (subseq octets header (+ header warn)) '(vector (unsigned-byte 8) *)))))
         (setf *callback-handler* (cons :succeed (subseq octets (+ header warn)))))))
    (otherwise
     (setf *callback-handler* (cons :failed (foreign-string-to-lisp errormsg)))))
  nil)

(defun round-up (n multiple)
  (multiple-value-bind (quotient remainder) (truncate n multiple)
    (if (zerop remainder) n (* (1+ quotient) multiple))))

(defun make-request-form (src params)
  (let* ((src-encoded (babel:string-to-octets src :encoding :utf-8))
         (src-padded-len (round-up (1+ (length src-encoded)) 4))
         (src-padding-len (- src-padded-len (length src-encoded)))
         (src-padded (concatenate
                      '(vector (unsigned-byte 8))
                      src-encoded
                      (make-array src-padding-len :element-type '(unsigned-byte 8) :initial-element 0)))
         (params-encoded (babel:string-to-octets params :encoding :utf-8))
         (params-padded (concatenate
                         '(vector (unsigned-byte 8))
                         params-encoded
                         (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
         (header (map 'list #'char-code (cl-pack:pack "<QQ" (length src-padded) (length params-padded)))))
    (concatenate '(vector (unsigned-byte 8)) header src-padded params-padded)))

(defun mtl-compile-source (source
                           &key
                             (fmodules-cache-path
                              #+darwin(progn "~/Library/Caches")
                              #-darwin(progn "~/.cache"))
                           &aux
                             (service (MTLCodeGenServiceCreate "caten"))
                             (params (format nil "-fno-fast-math -std=metal3.1 --driver-mode=metal -x metal -fmodules-cache-path=~a -fno-caret-diagnostics" fmodules-cache-path))
                             (*callback-handler* :ready))
  (declare (type foreign-pointer service) (type string source params))
  (let ((request (make-request-form source params))
        (callback (make-callback-closure (callback callback))))
    (with-pointer-to-vector-data (*request request)
      (MTLCodeGenServiceBuildRequest
       service (null-pointer) +request-type-compile+
       *request (length request) callback))
    (free-callback-closure callback)
    (assert (consp *callback-handler*) () "*callback-handler* did not receive anything!")
    (case (car *callback-handler*)
      (:succeed
       (let* ((len (length (cdr *callback-handler*)))
              (octets (make-array len :element-type '(unsigned-byte 8) :initial-contents (cdr *callback-handler*))))
         (assert (string= "MTLB" (flexi-streams:octets-to-string (subseq octets 0 4))) () "Invalid Metal library. Corrupt XCode?")
         (assert (string= "ENDT" (flexi-streams:octets-to-string (subseq octets (- len 4)))) () "Invalid Metal library. Corrupt XCode?")
         octets))
      (:failed
       (error "Failed to compile a metallib:~%~a~%Compiled with this command: ~a" (cdr *callback-handler*) params)))))
;; ~~ MetalBuffer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass MetalBuffer () ((value :accessor buffer-value) (shape :accessor buffer-shape) (dtype :accessor buffer-dtype)))
(defclass MetalRuntime () ((device :accessor metal-runtime-device)))

(defun dtype/size-of (dtype)
  "Return: bit"
  (ecase dtype
    (:float64 64)
    (:float32 32)
    (:float16 16)
    (:bfloat16 16)
    (:uint64 64)
    (:int64  64)
    (:uint32 32)
    (:int32 32)
    (:int16 16)
    (:uint16 16)
    (:uint8 8)
    (:int8 8)
    (:bool 1)))

(defun ->cffi-dtype (dtype)
  (ecase dtype
    (:bool :bool)
    (:float64 :double)
    (:float32 :float)
    (:uint64 :uint64)
    (:int64 :int64)
    (:int32 :int32)
    (:uint32 :uint32)
    (:int16 :int16)
    (:uint16 :uint16)
    (:uint8 :uint8)
    (:int8 :int8)))

(defun dtype->lisp (dtype)
  "to which dtypes values are coerced?"
  (case dtype
    (:float64 'double-float)
    (:float32 'single-float)
    (:float16 'single-float)
    (:bfloat16 'single-float)
    (:uint64  '(unsigned-byte 64))
    (:int64   '(signed-byte 64))
    (:uint32  '(unsigned-byte 32))
    (:int32   '(signed-byte 32))
    (:uint16  '(unsigned-byte 16))
    (:int16   '(signed-byte 16))
    (:uint8   '(unsigned-byte 8))
    (:int8    '(signed-byte 8))
    (:bool    'boolean)
    (otherwise (error "dtype->lisp: ~a is not supported" dtype))))

(defmethod initialize-instance :after ((runtime MetalRuntime) &key)
  (ensure-foreign-library)
  (with-float-traps-masked t
    (setf (metal-runtime-device runtime) (MTLCreateSystemDefaultDevice))))

(defmethod open-buffer ((runtime MetalRuntime) (buffer MetalBuffer))
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool) nil 0.0))
        (size (* (buffer-shape buffer) (dtype/size-of (buffer-dtype buffer)))))
    (if (= 0 (length (buffer-shape buffer)))
        (setf (buffer-value buffer) initial-value)
        (setf (buffer-value buffer) (msg (metal-runtime-device runtime) "newBufferWithLength:options:" :pointer :ulong size :int 0)))))

(defmethod close-buffer ((runtime MetalRuntime) (buffer MetalBuffer))
  (when (pointerp (buffer-value buffer))
    (msg (buffer-value buffer) "release" :void)
    (setf (buffer-value buffer) nil)))

(defmethod transfer-from-array ((runtime MetalRuntime) (buffer MetalBuffer) array)
  ;; CPU -> METAL
  (let ((val (msg (buffer-value buffer) "contents" :pointer)))
    (dotimes (i (apply #'* (buffer-shape buffer)))
      (setf (mem-aref val (->cffi-dtype (buffer-dtype buffer)) i) (aref array i)))))

(defmethod transfer-into-array ((buffer MetalBuffer))
  ;; METAL -> CPU
  (when (numberp (buffer-value buffer))
    (return-from transfer-into-array (buffer-value buffer)))
  (let ((val (msg (buffer-value buffer) "contents" :pointer))
        (placeholder (make-array (buffer-shape buffer) :element-type (dtype->lisp (buffer-dtype buffer)))))
    (dotimes (i (apply #'* (buffer-shape buffer)) placeholder)
      (setf (aref placeholder i) (mem-aref val (->cffi-dtype (buffer-dtype buffer)) i)))))
;; ~~ Compiler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun header ()
  (format nil "
#include <metal_stdlib>
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
using namespace metal;
#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))
"))

(defclass Metal-Program ()
  ((lib :initarg :lib :accessor mp-lib)
   (device :initarg :device :accessor mp-device)
   (mtl-queue :initarg :mtl-queue :accessor mp-mtl-queue)
   (name :initarg :name :accessor mp-name)
   (library :accessor mp-library)
   (fxn :accessor mp-fxn)
   (global-size :initarg :global-size :accessor mp-global-size)
   (local-size :initarg :local-size :accessor mp-local-size)
   (pipeline-state :accessor mp-pipeline-state)))

(defmethod initialize-instance :after ((mp Metal-Program) &key)
  (assert (string= "MTLB" (flexi-streams:octets-to-string (subseq (mp-lib mp) 0 4))) () "Invalid Metal library. Corrupt XCode?")
  (with-pointer-to-vector-data (*lib (mp-lib mp))
    (let ((data (dispatch-data-create *lib (length (mp-lib mp)) (null-pointer) (null-pointer)))
          (error-ptr (null-pointer)))
      (setf (mp-library mp) (msg (mp-device mp) "newLibraryWithData:error:" :pointer :pointer data :pointer error-ptr))
      (assert (null-pointer-p error-ptr) () "Failed to create a Metal library: ~a" (msg error-ptr "localizedDescription" :pointer))
      (setf (mp-fxn mp) (msg (mp-library mp) "newFunctionWithName:" :pointer :pointer (to-ns-str (string-downcase (princ-to-string (mp-name mp))))))
      (let ((descriptor (msg (objc-getclass "MTLComputePipelineDescriptor") "new" :pointer)))
        (assert (not (null-pointer-p (mp-fxn mp))) () "setComputeFunction: function must not be a null pointer! looks like the compilation was failed?")
        (msg descriptor "setComputeFunction:" :void :pointer (mp-fxn mp))
        (msg descriptor "setSupportIndirectCommandBuffers:" :void :bool t)
        (let ((error-ptr (null-pointer)))
          (setf (mp-pipeline-state mp) (msg (mp-device mp) "newComputePipelineStateWithDescriptor:options:reflection:error:" :pointer :pointer descriptor :int 1 :pointer (null-pointer) :pointer error-ptr))
          (assert (null-pointer-p error-ptr) () "Failed to create a Metal pipeline state: ~a" (msg error-ptr "localizedDescription" :pointer)))))))

(defcstruct MTLSize (width :ulong) (height :ulong) (depth :ulong))
(defun load-size (mtl-size width height depth)
  (setf (foreign-slot-value mtl-size '(:struct mtlsize) 'width) width
        (foreign-slot-value mtl-size '(:struct mtlsize) 'height) height
        (foreign-slot-value mtl-size '(:struct mtlsize) 'depth) depth))

(defun cmdbuf-start-time (cmdbuf) (msg cmdbuf "GPUStartTime" :double))
(defun cmdbuf-end-time (cmdbuf) (msg cmdbuf "GPUEndTime" :double))
(defun cmdbuf-elapsed-time (cmdbuf) (- (cmdbuf-end-time cmdbuf) (cmdbuf-start-time cmdbuf)))

(defmethod invoke ((mp Metal-Program) &rest buffers)
  (assert (= (length buffers) (length (mp-argtypes mp))) () "Metal: The number of arguments does not match the number of arguments in the Metal program.")
  (let ((total-max-threads (msg (mp-pipeline-state mp) "maxTotalThreadsPerThreadgroup" :int)))
    (when (> (apply #'* (mp-local-size mp)) total-max-threads) (error "Error: TODO"))
    (let* ((command-buffer (msg (mp-mtl-queue mp) "commandBuffer" :pointer))
           (encoder (msg command-buffer "computeCommandEncoder" :pointer)))
      (msg encoder "setComputePipelineState:" :void :pointer (mp-pipeline-state mp))
      (loop for buf in buffers
            for nth upfrom 0 do
              (msg encoder "setBuffer:offset:atIndex:" :void :pointer (buffer-value buf) :int 0 :int nth))
      (assert (= (length (mp-local-size mp)) 3) () "Metal only supports for 3d parallelism!")
      (with-foreign-objects ((gs '(:struct MTLSize)) (ls '(:struct MTLSize)))
        (apply #'load-size gs (mp-global-size mp))
        (apply #'load-size ls (mp-local-size mp))
        (msg encoder "dispatchThreadgroups:threadsPerThreadgroup:" :void :pointer gs :pointer ls))
      (msg encoder "endEncoding" :void)
      (msg command-buffer "setLabel:" :void :pointer (to-ns-str (string-downcase (princ-to-string (mp-name mp)))))
      (msg command-buffer "commit" :void)
      (msg command-buffer "waitUntilCompleted" :void)
      (let ((err (msg command-buffer "error" :pointer)))
        (assert (null-pointer-p err) () "Failed to execute a Metal command buffer: ~a" (msg err "localizedDescription" :pointer)))
      (coerce (cmdbuf-elapsed-time command-buffer) 'single-float))))
;; ~~ Entry Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun mcompile (fname source &key (grid-size `(1 1 1)) (thread-size `(1 1 1)))
  (declare (type string fname source))
  (with-float-traps-masked t
    (let* ((lib (mtl-compile-source source))
           (device (MTLCreateSystemDefaultDevice))
           (mtl-queue (msg device "newCommandQueueWithMaxCommandBufferCount:" :pointer :int 1024)))
      (make-instance
       'Metal-Program :lib lib :name fname :device device :mtl-queue mtl-queue :global-size grid-size :local-size thread-size))))

(defmacro defmkernel ((fname lisp-name) source)
  `(defun ,lisp-name (&key (grid-size '(1 1 1)) (thread-size '(1 1 1)))
     (mcompile ,fname ,source :grid-size grid-size :thread-size thread-size)))
;; ~~ MPS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod profile-on-gemm ((prg Metal-Program))
  ;; N moves from zero to max
  ;; Finally compares the result w/ numcl
  )
