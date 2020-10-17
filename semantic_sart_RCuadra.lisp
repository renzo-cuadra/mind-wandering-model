;;
;;	Semantic Sustained Attention to Response Task (SART)
;;
;;	In each trial, the participant sees a word that is either a fruit or an animal.
;;	They must press a key every time a fruit appears (89% of trials),
;;	but withhold their response when the stimulus is an animal (11% of trials).
;;
;;	The model can be run with the following commands:
;;	(do-trial-apple) --> to run a target trial
;;	(do-trial-dog) --> to run a non-target trial
;;	(do-sart) --> to run one block of trials for 1 participant
;;	(do-sart-n N) --> to run one block of trials for N participants
;;
;;	Renzo Cuadra (2020)
;;	cuadra.ren@gmail.com


;;===================;;
;;  Experiment code  ;;
;;===================;;


;; Experiment settings
(defvar *stimulus-duration* 0.3) ; number of seconds the stimulus is shown
(defvar *inter-stimulus-interval* 0.9) ; number of seconds between trials
(defvar *target-trials* 801) ; number of target trials
(defvar *non-target-trials* 99) ; number of non-target trials
(defvar *thought-probes* 480) ; number of thought probes to be presented

(defvar *output-directory* "~/Documents/thirdyear/block3/CognitiveModelling/output/") ; location where output files are stored
(defvar *trace-file-name* "sart-trace") ; name of file in which the trace is stored
(defvar *visible* nil) ; visibility of the experiment window

;; Global variables for data storage
(defvar *stimuli* nil)
(defvar *fruits* (list "apple" "orange" "banana" "grape" "pear" "peach" "strawberry" "kiwi" "pineapple" "watermelon" "tomato" "plum" "grapefruit" "mango" "cherry"))
(defvar *animals* (list "dog" "cat" "horse" "lion" "bear" "tiger" "cow" "elephant" "deer" "mouse" "pig" "rat" "giraffe" "squirrel" "rabbit"))
(defvar *targets* nil)
(defvar *non-targets* nil)
(defvar *trial-response* nil)
(defvar *trial-start* nil)
(defvar *trial-rt* nil)
(defvar *trial-done* nil)
(defvar *all-responses* nil)
(defvar *all-rts* nil)
(defvar participant)

;; Do SART experiment n times, save trace to output file
(defun do-sart-n (n)
	(with-open-file
		(*standard-output*
			(ensure-directories-exist
				(merge-pathnames
					(make-pathname :name *trace-file-name* :type "txt")
					*output-directory*
				)
            )
			:direction :output :if-does-not-exist :create :if-exists :supersede
		)

		(setf *visible* nil)
		(format t "Running ~a model participants~%" n)
		(dotimes (i n)
			(setf participant (1+ i))
			(format t "Run ~a...~%" participant)
			(do-sart)
			(write-results-to-file (concatenate 'string "dat" (write-to-string participant)) participant *stimuli* (reverse *all-responses*) (reverse *all-rts*))
		)
		(format t "Done~%")
	)
)

;; Create a list with n randomly selected elements from a source list
(defun fill-list (n list)
	(loop repeat n collect (nth (random (length list)) list))
)

;; Do SART experiment 1 time
(defun do-sart ()
	(reset)
	(setf *all-responses* nil)
	(setf *all-rts* nil)
	(setf *targets* (fill-list *target-trials* *fruits*))
	(setf *non-targets* (fill-list *non-target-trials* *animals*))
	(setf *stimuli* (permute-list (concatenate 'list *targets* *non-targets* (make-array *thought-probes* :initial-element "X"))))
	(setf *visible* nil)
	(loop for stim in *stimuli* do (run-trial stim))
)

;; Do a single SART trial with a target stimulus
(defun do-trial-apple ()
	(setf *visible* t)
	(run-trial "apple")
)

;; Do a single SART trial with a non-target stimulus
(defun do-trial-dog ()
	(setf *visible* t)
	(run-trial "dog")
)

;; Execute a trial with a given stimulus
(defun run-trial (stim)
  (let ((window (open-exp-window "SART Experiment"
                                 :visible *visible*
                                 :width 300
                                 :height 300
                                 :x 300
                                 :y 300))
        )

    (add-text-to-exp-window :text stim
                              :width 30
							  :height 30
                              :x 145
                              :y 150)

    (setf *trial-response* nil)
    (setf *trial-start* (get-time))
    (setf *trial-rt* nil)
	(setf *trial-done* nil)

	(install-device window)
	(proc-display)
	(run-full-time *stimulus-duration* :real-time *visible*)
	(allow-event-manager window)
	(clear-exp-window)
	(proc-display)
	(run-full-time *inter-stimulus-interval* :real-time *visible*))

	(push *trial-response* *all-responses*)
	(push *trial-rt* *all-rts*)
)

;; Register the model's key presses
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *trial-rt* (/ (- (get-time) *trial-start*) 1000.0))
  (setf *trial-response* (string key))
  (setf *trial-done* t)
)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant stimuli responses rts)
	(with-open-file
		(out
			(ensure-directories-exist
				(merge-pathnames
					(make-pathname :name name :type "csv")
					*output-directory*
				)
			)
			:direction :output :if-does-not-exist :create :if-exists :supersede
		)
		(format out "participant, trial, stimulus, response, rt~%")
		(loop
			for trial from 1
			for stimulus in stimuli
			for response in responses
			for rt in rts
			if(string= stimulus "X")
				do (format out "~a, ~a, ~a, ~a, ~a~%" participant trial stimulus response rt)
		)
	)
)



;;===================;;
;;    Model code     ;;
;;===================;;

(clear-all)

(define-model sart

;; Model parameters
(sgp :v nil ; main trace detail
	:act low ; activation trace detail
	:sact t ; include activation trace in main trace
	:show-focus t ; show where the model is looking
	:esc t ; enable sub-symbolic level
	:rt -3 ; retrieval threshold
	:bll 0.1 ; base-level learning
	:ol nil ; optimized learning on
	:mas 2 ; spreading activation on
	:ans 0.2 ;activation noise
	:er t ; ties between chunks are broken randomly
)

(chunk-type beginning label)
(chunk-type goal state key)
(chunk-type subgoal step)
(chunk-type srmapping stimulus key)
(chunk-type word string meaning)
(chunk-type object title category)
(chunk-type memory id)

(add-dm
	(start isa chunk)
	(startgoal isa beginning label start)
	(attend isa goal state attend key 1)
	(wander isa goal state wander key 0)
	(read-word isa subgoal step read-word)
	(recognise-category isa subgoal step recognise-category)
	(get-response isa subgoal step get-response)
	(make-response isa subgoal step make-response)
	(remember isa subgoal step remember)
	(press-on-target isa srmapping stimulus fruit key a)
	(withhold-on-non isa srmapping stimulus animal key nil)
	(answer-probe isa srmapping stimulus thoughtprobe key x)

	;; Chunks for string-to-object mappings
	(map-tp isa word string "X" meaning tp)
	(map-apple isa word string "apple" meaning apple)
	(map-orange isa word string "orange" meaning orange)
	(map-banana isa word string "banana" meaning banana)
	(map-grape isa word string "grape" meaning grape)
	(map-pear isa word string "pear" meaning pear)
	(map-peach isa word string "peach" meaning peach)
	(map-strawberry isa word string "strawberry" meaning strawberry)
	(map-kiwi isa word string "kiwi" meaning kiwi)
	(map-pineapple isa word string "pineapple" meaning pineapple)
	(map-watermelon isa word string "watermelon" meaning watermelon)
	(map-tomato isa word string "tomato" meaning tomato)
	(map-plum isa word string "plum" meaning plum)
	(map-grapefruit isa word string "grapefruit" meaning grapefruit)
	(map-mango isa word string "mango" meaning mango)
	(map-cherry isa word string "cherry" meaning cherry)
	(map-dog isa word string "dog" meaning dog)
	(map-cat isa word string "cat" meaning cat)
	(map-horse isa word string "horse" meaning horse)
	(map-lion isa word string "lion" meaning lion)
	(map-bear isa word string "bear" meaning bear)
	(map-tiger isa word string "tiger" meaning tiger)
	(map-cow isa word string "cow" meaning cow)
	(map-elephant isa word string "elephant" meaning elephant)
	(map-deer isa word string "deer" meaning deer)
	(map-mouse isa word string "mouse" meaning mouse)
	(map-pig isa word string "pig" meaning pig)
	(map-rat isa word string "rat" meaning rat)
	(map-giraffe isa word string "giraffe" meaning giraffe)
	(map-squirrel isa word string "squirrel" meaning squirrel)
	(map-rabbit isa word string "rabbit" meaning rabbit)

	;; Chunks for object-to-category mappings
	(tp isa object title tp category thoughtprobe)
	(apple isa object title apple category fruit)
	(orange isa object title orange category fruit)
	(banana isa object title banana category fruit)
	(grape isa object title grape category fruit)
	(pear isa object title pear category fruit)
	(peach isa object title peach category fruit)
	(strawberry isa object title strawberry category fruit)
	(kiwi isa object title kiwi category fruit)
	(pineapple isa object title pineapple category fruit)
	(watermelon isa object title watermelon category fruit)
	(tomato isa object title tomato category fruit)
	(plum isa object title plum category fruit)
	(grapefruit isa object title grapefruit category fruit)
	(mango isa object title mango category fruit)
	(cherry isa object title cherry category fruit)
	(dog isa object title dog category animal)
	(cat isa object title cat category animal)
	(horse isa object title horse category animal)
	(lion isa object title lion category animal)
	(bear isa object title bear category animal)
	(tiger isa object title tiger category animal)
	(cow isa object title cow category animal)
	(elephant isa object title elephant category animal)
	(deer isa object title deer category animal)
	(mouse isa object title mouse category animal)
	(pig isa object title pig category animal)
	(rat isa object title rat category animal)
	(giraffe isa object title giraffe category animal)
	(squirrel isa object title squirrel category animal)
	(rabbit isa object title rabbit category animal)

	;; Chunks for populating memories
	(pay-attention isa memory id pay-attention)
	(mem1 isa memory id mem1)
	(mem2 isa memory id mem2)
	(mem3 isa memory id mem3)
	(mem4 isa memory id mem4)
	(mem5 isa memory id mem5)
	(mem6 isa memory id mem6)
	(mem7 isa memory id mem7)
	(mem8 isa memory id mem8)
	(mem9 isa memory id mem9)
	(mem10 isa memory id mem10)
	(mem11 isa memory id mem10)
	(mem12 isa memory id mem10)
	(mem13 isa memory id mem10)
	(mem14 isa memory id mem10)
	(mem15 isa memory id mem10)
	(mem16 isa memory id mem10)
	(mem17 isa memory id mem10)
	(mem18 isa memory id mem10)
	(mem19 isa memory id mem10)
	(mem20 isa memory id mem10)
	(mem21 isa memory id mem10)
	(mem22 isa memory id mem10)
	(mem23 isa memory id mem10)
	(mem24 isa memory id mem10)
	(mem25 isa memory id mem10)
)

(set-base-levels
	(attend				5000	0)
	(wander         	7000	-10000)
	(press-on-target	10000	-10000)
	(withhold-on-non	10000	-10000)
	(answer-probe		10000	-10000)
	(map-tp 			10000	-10000)
	(map-apple			10000	-10000)
	(map-orange			10000	-10000)
	(map-banana			10000	-10000)
	(map-grape			10000	-10000)
	(map-pear			10000	-10000)
	(map-peach			10000	-10000)
	(map-strawberry		10000	-10000)
	(map-kiwi			10000	-10000)
	(map-pineapple		10000	-10000)
	(map-watermelon		10000	-10000)
	(map-tomato			10000	-10000)
	(map-plum			10000	-10000)
	(map-grapefruit		10000	-10000)
	(map-mango			10000	-10000)
	(map-cherry			10000	-10000)
	(map-dog			10000	-10000)
	(map-cat			10000	-10000)
	(map-horse			10000	-10000)
	(map-lion			10000	-10000)
	(map-bear			10000	-10000)
	(map-tiger			10000	-10000)
	(map-cow			10000	-10000)
	(map-elephant		10000	-10000)
	(map-deer			10000	-10000)
	(map-mouse			10000	-10000)
	(map-pig			10000	-10000)
	(map-rat			10000	-10000)
	(map-giraffe		10000	-10000)
	(map-squirrel		10000	-10000)
	(map-rabbit			10000	-10000)
	(tp 				10000	-10000)
	(apple				10000	-10000)
	(orange				10000	-10000)
	(banana				10000	-10000)
	(grape				10000	-10000)
	(pear				10000	-10000)
	(peach				10000	-10000)
	(strawberry			10000	-10000)
	(kiwi				10000	-10000)
	(pineapple			10000	-10000)
	(watermelon			10000	-10000)
	(tomato				10000	-10000)
	(plum				10000	-10000)
	(grapefruit			10000	-10000)
	(mango				10000	-10000)
	(cherry				10000	-10000)
	(dog				10000	-10000)
	(cat				10000	-10000)
	(horse				10000	-10000)
	(lion				10000	-10000)
	(bear				10000	-10000)
	(tiger				10000	-10000)
	(cow				10000	-10000)
	(elephant			10000	-10000)
	(deer				10000	-10000)
	(mouse				10000	-10000)
	(pig				10000	-10000)
	(rat				10000	-10000)
	(giraffe			10000	-10000)
	(squirrel			10000	-10000)
	(rabbit				10000	-10000)
	(pay-attention		300		-10000)
	(mem1				500		-10000)
	(mem2				500		-10000)
	(mem3				500		-10000)
	(mem4				500		-10000)
	(mem5				500		-10000)
	(mem6				500		-10000)
	(mem7				500		-10000)
	(mem8				500		-10000)
	(mem9				500		-10000)
	(mem10				500		-10000)
	(mem11				500		-10000)
	(mem12				500		-10000)
	(mem13				500		-10000)
	(mem14				500		-10000)
	(mem15				500		-10000)
	(mem16				500		-10000)
	(mem17				500		-10000)
	(mem18				500		-10000)
	(mem19				500		-10000)
	(mem20				500		-10000)
	(mem21				500		-10000)
	(mem22				500		-10000)
	(mem23				500		-10000)
	(mem24				500		-10000)
	(mem25				500		-10000)
)

;; Start by setting the goal to attend
(p start-task
	=goal>
		isa			beginning
		label		start
	?retrieval>
		buffer		empty
		state		free
	-	state		error
==>
	+retrieval>
		isa			goal
		state		attend
	-goal>
)

;; Clear buffers and retrieve a new goal
(p check-current-goal
	=retrieval>
		isa			goal
		state		attend
	?retrieval>
		state		free
	-	state		error
	?goal>
		buffer 		empty
	?visual>
	-	scene-change T
==>
	=retrieval>
		state		nil ; clear retrieval buffer without strengthening chunk
	-retrieval>
	+retrieval>
		isa			goal
	-	state		nil
)

;; Look at the stimulus presented on the screen
(p identify-stimulus
	?goal>
		buffer		empty
	=retrieval>
		isa			goal
		state		attend
	=visual-location>
	?visual>
		state		free
==>
	+visual>
		isa			move-attention
		screen-pos	=visual-location
	+goal>
		isa			subgoal
		step		read-word
	=retrieval>
		state		nil ; clear retrieval buffer without strengthening chunk
	-retrieval>
)

;; Read the word by retrieving a string-to-object mapping
(p read-word
	=goal>
		isa			subgoal
		step		read-word
	=visual>
		isa			text
		value		=word
	?visual>
		state		free
	?retrieval>
		state		free
==>
	+retrieval>
		isa			word
		string		=word
		!output!	(=word)
	+goal>
		isa			subgoal
		step		recognise-category
	+visual>
		isa			clear-scene-change
)

;; Recognise word category by retrieving an object-to-category mapping
(p recognise-category
	=goal>
		isa			subgoal
		step		recognise-category
	=retrieval>
		isa			word
		meaning		=obj
==>
	+retrieval>
		isa			object
		title		=obj
	+goal>
		isa			subgoal
		step		get-response
)

;; Retrieve correct response according to word category
(p retrieve-response
	=goal>
		isa			subgoal
		step		get-response
	=retrieval>
		isa			object
		category	=categ
==>
	+retrieval>
		isa			srmapping
		stimulus	=categ
	+goal>
		isa			subgoal
		step		make-response
)

;; Press key if word is a fruit
(p respond-if-target
	=goal>
		isa			subgoal
		step		make-response
	=retrieval>
		isa			srmapping
		stimulus	fruit
		key			=key
	?manual>
		state		free
==>
	+manual>
		isa			press-key
		key			=key
	-goal>
	-visual-location>
	-visual>
	+retrieval>
		isa			goal
	-	state		nil
)

;; Withhold if word is an animal
(p withhold-if-non-target
	=goal>
		isa			subgoal
		step		make-response
	=retrieval>
		isa			srmapping
		stimulus	animal
		key			nil
	?manual>
		state		free
==>
	-goal>
	-visual-location>
	-visual>
	+retrieval>
		isa			goal
	-	state		nil
)

(p answer-probe
	=goal>
		isa			subgoal
		step		make-response
	=retrieval>
		isa			srmapping
		stimulus	thoughtprobe
		key			=key
	?manual>
		state		free
==>
	+manual>
		isa			press-key
		key			=key
	-goal>
	-visual-location>
	-visual>
	+retrieval>
		isa			goal
	-	state		nil
	)

;;;;; Production rules for mind-wandering ;;;;;;

;; Start retrieving memories if current goal is to wander
(p start-wandering
	=retrieval>
		isa			goal
		state		wander
	?goal>
		buffer		empty
==>
	+goal>
		isa			subgoal
		step		remember
	+retrieval>
     	isa 		memory
	-	id			nil
     	:recently-retrieved nil
)

;; Retrieve a new memory if pay-attention has not been retrieved
(p retrieve-memories
	=goal>
		isa			subgoal
		step		remember
	=retrieval>
		isa			memory
	-	id			pay-attention
	?visual>
	-	scene-change T
==>
	+retrieval>
     	isa 		memory
	-	id			nil
     	:recently-retrieved nil
)

;; Switch to attend goal if pay-attention is retrieved
(p remember-to-attend
	=goal>
     	isa			subgoal
	 	step		remember
	=retrieval>
		isa			memory
		id			pay-attention
==>
  	=retrieval>
		state		nil ; clear retrieval buffer without strengthening chunk
	-retrieval>
	+retrieval>
		isa			goal
		state		attend
	-goal>
)

;; Press key as default response if a stimulus appears during mind-wandering
(p default-response	
	=goal>
		isa			subgoal
		step		remember
	?visual>
		scene-change T
	?visual-location>
	-	buffer 		empty
	?manual>
		state 		free
==>
	+manual>
		isa			press-key
		key			w
		;;!output!	(=word)
	-goal>
	-visual-location>
	-visual>
	-retrieval>
	+retrieval>
		isa			goal
	-	state		nil
)

(goal-focus startgoal)

)

