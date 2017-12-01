;; TODOS:
;;   - Figure out how to deal w/ duplicate states
;;   - Fix while loop (must sort nodes before choosing node to pop)
;;   - Function to back track and get solution from hash adjustable
;;   - Print and format solution

;; Struct for current state
(defstruct state
   start_bank    ;; Array(2) defining num m and c on start bank
   end_bank      ;; Array(2) defining num m and c on end bank
   boat_pos      ;; Position of boat
)

;; Struct for a single node in the tree
(defstruct node
   state         ;; Struct of current state
   id            ;; ID num of node
   parent_id     ;; ID num of parent node
   heuristic     ;; Represents heuristic value
   depth         ;; Depth of the node
)

;; Array for new states
(setf state-vector (make-array 0 :fill-pointer 0 :adjustable t))

;; Hashtable for visited states
(setf visited (make-hash-table))

;; Vector to store unique states
(setf unique-states(make-array 0 :fill-pointer 0 :adjustable t))

;; Total missionaries
(setf tot_m 15)

;; Total Cannibals
(setf tot_c 15)

;; Total boat size
(setf boat_size 6)

;; Counter for IDs
(setf counter 0)


;; Initialize node
;; Takes in total number of missionaries, total number of cannibals, and boat size
(defun init_node(tot_m tot_c boat_size)
   (print "Initializing node...")

   (setf start_bank (make-array '(2)))
   (setf (aref start_bank 0) tot_m)
   (setf (aref start_bank 1) tot_c)

   (setf end_bank (make-array '(2)))
   (setf (aref end_bank 0) 0)
   (setf (aref end_bank 1) 0)

   (setf start_node(make-node :start_bank start_bank :end_bank end_bank :boat_pos 0 :id (incf counter) :parent_id 0 :heuristic 30 :depth 1))

   (vector-push-extend start_node state-vector)
)

;; Desc: Finds all valid moves for the current state node and creates new nodes
;; Parameters: Takes a current state_node (struct)
(defun find_moves(curr_state)
   (setf boat_pos (node-boat_pos curr_state))
   (if (= 0 boat_pos)
      (progn
         (setf start_bank(node-start_bank curr_state))
         (setf total (+(aref start_bank 0)(aref start_bank 1)))
      )
      (progn
         (setf end_bank (node-end_bank curr_state))
         (setf total (+(aref end_bank 0)(aref end_bank 1)))
      )
   )

   (if (< total boat_size)
      (setf x total)
      (setf x boat_size)
   )

   (loop for j from 1 to x do
      (loop for c from 0 to j do
         (if (or(<= c (/ j 2))(= c j))
           (transition curr_state (- j c) c)
         )
      )
   )
)

;; Desc: Creates a new node given info
;; Parameters (m,c) pair and current state node
(defun transition(curr_node m c)

   ; Calculate new values on bank start and end bank
   (setf boat_pos(node-boat_pos curr_node))

   (setf curr_start_bank (node-start_bank curr_node))
   (setf curr_end_bank (node-end_bank curr_node))

   (setf new_start_bank (make-array '(2)))
   (setf new_end_bank (make-array '(2)))

   (if (= 0 boat_pos)
      (progn
         (setf (aref new_start_bank 0) (- (aref curr_start_bank 0) m))
         (setf (aref new_start_bank 1) (- (aref curr_start_bank 1) c))

         (setf (aref new_end_bank 0) (+ (aref curr_end_bank 0) m))
         (setf (aref new_end_bank 1) (+ (aref curr_end_bank 1) c))
      )
      (progn
         (setf (aref new_start_bank 0) (+ (aref curr_start_bank 0) m))
         (setf (aref new_start_bank 1) (+ (aref curr_start_bank 1) c))

         (setf (aref new_end_bank 0) (- (aref curr_end_bank 0) m))
         (setf (aref new_end_bank 1) (- (aref curr_end_bank 1) c))
      )
   )

   ; Check validity of new node
   ;; TODO: Update to call is unique here
   (if (isValid new_start_bank new_end_bank)
      (create_node new_start_bank new_end_bank boat_pos (node-id curr_node) curr_node)
   )

)

;; Desc: Checks the validity of a new node
(defun isValid(start_bank end_bank )
   (setf start_m (aref start_bank 0))
   (setf start_c (aref start_bank 1))

   (setf end_m (aref end_bank 0))
   (setf end_c (aref end_bank 1))

   (if (and (or (= start_m 0)(<= start_c start_m))(or(= end_m 0)(<= end_c end_m)))
     (if (and(and (<= 0 start_m)(<= 0 start_c))(and (<= 0 end_m)(<= 0 start_c)))
      (return-from isValid 't)
     )
   )
)

;; Checks whether node has been expanded before
;; Pushes unique states on to array
(defun isUnique(s_bank end_bank boat_pos)
   (setf new_state(make-state :start_bank s_bank :end_bank e_bank :boat_pos 0))

)

;; Desc: Initialize new node
(defun create_node(s_bank e_bank boat_pos id curr_node)
   (if (= 0 boat_pos)
      (setf boat_pos 1)
      (setf boat_pos 0)
   )
   ;(setf h (+ (aref s_bank 0)(aref s_bank 1)))
   (setf h (+ (aref s_bank 0)(aref s_bank 1)))
   (setf h (+ h boat_pos))
   (setf h (* h (+ 1(node-depth curr_node))))
   (setf new_node(make-node :start_bank s_bank :end_bank e_bank :boat_pos boat_pos :id (incf counter) :parent_id id :heuristic h :depth (+ 1(node-depth curr_node))))

   (vector-push-extend new_node state-vector)
)

;; Desc: Checks if the next node to be expanded is at the goal state
;; TODO: Something here when goal state is reached
(defun check_goal_state(curr_node)
  (setf node_end(node-end_bank curr_node))
  (setf m_end (aref node_end 0))
  (setf c_end (aref node_end 1))

   (if (and(= m_end tot_m)(= c_end tot_c))
      (print here)
   )
)

;; Desc: function to sort state vector based off heuristic
;; Simple selection sort
(defun sort_nodes(state-vector)

    (setf l (length state-vector))

    (loop for i from 0 to (- l 1) do
       (setf min i)
       (loop for j from (+ i 1) to (- l 1) do

          (setf curr_s (aref state-vector j))
          (setf min_s (aref state-vector min))

          (if (> (node-heuristic curr_s) (node-heuristic min_s))
             (setf min j)
          )
       )
       ;;Swap
       (setf tmp (aref state-vector i))
       (setf (aref state-vector i) (aref state-vector min))
       (setf (aref state-vector min) tmp)
    )
)


;; TODO: Make a solution finder
;; Desc: Backtracks and stores the final solution path

;; Main function that initializes and calls the solver
(defun main()
   (print "Missionaries vs. Cannibals")

   ;; Initialize first node
   (init_node tot_m tot_c boat_size)

   (setf y 0)
   (loop while(< y 100) do

      (if (< 0 (length state-vector))
        (progn
          (print "Sorting nodes...")
          (sort_nodes state-vector)
        )
      )

      (setf curr_state (vector-pop state-vector))
      (print "Expand node")
      (print curr_state)

      (find_moves curr_state)

      (setf (gethash (node-id curr_state) visited)curr_state)

      ;;If goal state then stop and get solution
      ;;Expand other nodes
      (incf y)
   )

)