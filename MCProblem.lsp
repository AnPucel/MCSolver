;----------------------------------------
; Missionaries vs. Cannibals Problem
;----------------------------------------

; Struct for current state defining the state of the problem
(defstruct state
   start_bank    ;; Array(2) defining num m and c on start bank
   end_bank      ;; Array(2) defining num m and c on end bank
   boat_pos      ;; Position of boat
)

;; Struct for a single node in the tree
(defstruct node
   move          ;; Array(2) of the move sequence that resulted in the node
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

;; Array to store unique states
(setf unique-states(make-array 0 :fill-pointer 0 :adjustable t))

;; Total missionaries
(setf tot_m 15)

;; Total Cannibals
(setf tot_c 15)

;; Total boat size
(setf boat_size 6)

;; Counter for IDs
(setf counter 0)


;-------------------------------------------------------
; Name:     init_node
; Params:   initial number of missionaries, initial
;           number of cannibals, number the boat
;           can carry
; Desc:     - initializes the first node and state
;           - adds the new state to the unique states
;           - adds the new node to the state-vector
;--------------------------------------------------------
(defun init_node(tot_m tot_c boat_size)
   (setf start_bank (make-array '(2)))
   (setf (aref start_bank 0) tot_m)
   (setf (aref start_bank 1) tot_c)

   (setf end_bank (make-array '(2)))
   (setf (aref end_bank 0) 0)
   (setf (aref end_bank 1) 0)

   (setf state (make-state :start_bank start_bank :end_bank end_bank :boat_pos 0))
   (setf start_node(make-node :state state :id (incf counter) :parent_id 0 :heuristic 30 :depth 1))

   (vector-push-extend state unique-states)
   (vector-push-extend start_node state-vector)
)

;----------------------------------------------------------
; Name:     find_moves
; Params:   current node to find moves for
; Desc:     - Determines how many moves are feasible given
;             the current state
;           - For all plausible moves calls the transition
;             function
;----------------------------------------------------------
(defun find_moves(curr_node)

   (if (= 0 (state-boat_pos (node-state curr_node)))
      (progn
         (setf s_bank(state-start_bank(node-state curr_node)))
         (setf total (+(aref s_bank 0)(aref s_bank 1)))
      )
      (progn
         (setf e_bank(state-end_bank(node-state curr_node)))
         (setf total (+(aref e_bank 0)(aref e_bank 1)))
      )
   )

   (if (< total boat_size)
      (setf x total)
      (setf x boat_size)
   )

   (loop for j from 1 to x do
      (loop for c from 0 to j do
         (if (or(<= c (/ j 2))(= c j))
           (transition curr_node (- j c) c)
         )
      )
   )
)

;-------------------------------------------------------------
; Name:     transition
; Params:   current node, number of missionaries to be
;           transported, number of cannibals to be
;           transported
; Desc:     - Calculates new values for start and end e_bank
;             based off of m and c criteria and current node
;             boat position
;           - Calls isValid function to check validity of
;             newly created node
;           - Calls isUnique to check uniqueness of the newly
;             created node
;           - If the node is both unique and valid it calls
;             create node
;--------------------------------------------------------------
(defun transition(curr_node m c)

   (setf boat_pos(state-boat_pos (node-state curr_node)))

   (setf curr_s_bank(state-start_bank(node-state curr_node)))
   (setf curr_e_bank(state-end_bank(node-state curr_node)))

   (setf new_start_bank (make-array '(2)))
   (setf new_end_bank (make-array '(2)))

   (if (= 0 boat_pos)
      (progn
         (setf (aref new_start_bank 0) (- (aref curr_s_bank 0) m))
         (setf (aref new_start_bank 1) (- (aref curr_s_bank 1) c))

         (setf (aref new_end_bank 0) (+ (aref curr_e_bank 0) m))
         (setf (aref new_end_bank 1) (+ (aref curr_e_bank 1) c))
      )
      (progn
         (setf (aref new_start_bank 0) (+ (aref curr_s_bank 0) m))
         (setf (aref new_start_bank 1) (+ (aref curr_s_bank 1) c))

         (setf (aref new_end_bank 0) (- (aref curr_e_bank 0) m))
         (setf (aref new_end_bank 1) (- (aref curr_e_bank 1) c))
      )
   )

   ; Check validity of new node
   (if (and (isValid new_start_bank new_end_bank)(isUnique new_start_bank new_end_bank boat_pos))
      (create_node new_start_bank new_end_bank boat_pos (node-id curr_node) curr_node)
   )

)

;-------------------------------------------------------
; Name:     isValid
; Params:   start bank array of new node and end bank array
;           of new node
; Return:   returns true if is valid
; Desc:     - Checks that the number of missionaries is never
;             outnumbered by the number of cannibals at either
;             bank
;--------------------------------------------------------
(defun isValid(start_bank end_bank )
   (setf start_m (aref start_bank 0))
   (setf start_c (aref start_bank 1))

   (setf end_m (aref end_bank 0))
   (setf end_c (aref end_bank 1))

   (if (and (or (= start_m 0)(<= start_c start_m))(or(= end_m 0)(<= end_c end_m)))
     (if (and (and (<= 0 start_m)(<= 0 start_c))(and (<= 0 end_m)(<= 0 start_c)))
      (return-from isValid 't)
     )
   )
)

;------------------------------------------------------------
; Name:     isUnique
; Params:   start bank array, end bank array and Position
;           of boat to check uniqueness of
; Return:   returns true if given params form a unique states
;           returns nil if given params form a state that has
;           previously been expanded
; Desc:     - Loops through array of unique-states
;           - Checks to find if the given params is a state
;             that has already been expanded
;           - If the params form a unique state, a new state is
;             created and pushed to the unique-states vector
;------------------------------------------------------------
(defun isUnique(s_bank e_bank boat_pos)
  (if (= 0 boat_pos)
    (setf boat_pos 1)
    (setf boat_pos 0)
  )
   ;;If in unique states array don't expand
   (setf l (length unique-states))
   (loop for i from 0 to (- l 1) do
      (setf curr_state (aref unique-states i))
      (if (and (= (aref (state-start_bank curr_state) 0)(aref s_bank 0))
          (= (aref (state-start_bank curr_state) 1)(aref s_bank 1))
          (= (aref (state-end_bank curr_state) 0)(aref e_bank 0))
          (= (aref (state-end_bank curr_state) 1)(aref e_bank 1))
          (= (state-boat_pos curr_state) boat_pos)
          )
          (return-from isUnique nil)
      )
    )

   (setf new_state (make-state :start_bank s_bank :end_bank e_bank :boat_pos boat_pos))
   (vector-push-extend new_state unique-states)
   (return-from isUnique 't)
)

;-------------------------------------------------------
; Name:     create_node
; Params:   start bank array, end bank array, boat position
;           of new node to create, id of parent node, current
;           node to be expanded from
; Desc:     - Calculates heuristic for new node
;           - Sets all values for a new node
;           - Pushes the new node to the state-vector
;--------------------------------------------------------
(defun create_node(s_bank e_bank boat_pos id curr_node)
   (if (= 0 boat_pos)
      (setf boat_pos 1)
      (setf boat_pos 0)
   )
   ;(setf h (+ (aref s_bank 0)(aref s_bank 1)))
   (setf h (+ (aref s_bank 0)(aref s_bank 1)))
   (setf h (+ h boat_pos))
   (setf h (* h (+ 1(node-depth curr_node))))

   (setf new_state (make-state :start_bank s_bank :end_bank e_bank :boat_pos boat_pos))
   (setf new_node(make-node :state new_state :id (incf counter) :parent_id id :heuristic h :depth (+ 1(node-depth curr_node))))

   (vector-push-extend new_node state-vector)
)

;-------------------------------------------------------
; Name:     sort_nodes
; Params:   state-vector to sort
; Desc:     - utilizes a simple selection sort to sort
;             the nodes in the current state vector
;--------------------------------------------------------
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

;-------------------------------------------------------
; Name:     isGoal
; Params:   node to check if is at goal state
; Returns:  returns true if node is at the goal state
;           returns nil if node is not at the goal state
; Desc:     - checks if the current node is at the goal
;             state and returns
;--------------------------------------------------------
(defun isGoal(node)
   (setf state (node-state node))
   (if (and (= (aref (state-start_bank state) 0) 0)
    (= (aref (state-start_bank state) 1) 0)
    (= (aref (state-end_bank state) 0)tot_m)
    (= (aref (state-end_bank state) 1)tot_c)
    (= (state-boat_pos state) 1)
    )
      (return-from isGoal 't)
      (return-from isGoal nil)
    )
)

;-------------------------------------------------------
; Name:     getSolution
; Params:   the node that is at the goal state
; Desc:     - creates a new array to store the solution
;           - pushes the goal node to the solution vector
;           - loops until the parent_id = 1 (the start state)
;           - backtracks by using visited hashtable to get previous
;             node based off of parent id
;           - calls format solution once solution-vector is filled out
;--------------------------------------------------------
(defun getSolution(goal_node)
  (setf solution-vector (make-array 0 :fill-pointer 0 :adjustable t))
  (setf parent_id (node-parent_id goal_node))
  (vector-push-extend goal_node solution-vector)

  (loop while(not (= parent_id 1)) do
    (setf next_node(gethash parent_id visited))
    (setf parent_id (node-parent_id next_node))
    (vector-push-extend next_node solution-vector)
  )
  ;;TODO: add start state here
  (formatSolution solution-vector)
)

;-------------------------------------------------------
; Name:     formatSolution
; Params:   array of nodes that outline the solution
; Desc:     - loops through solution vector and formats
;             the output
;--------------------------------------------------------
(defun formatSolution(solution-vector)

  (setf l (length solution-vector))

  (loop for i from (- l 1) downto 0 do
    (setf something (aref solution-vector i))
    (setf else (node-state something))
    (format t "~% ~S ~S |~~~~~~~~~~~~~~| ~S ~S" (aref (state-start_bank else) 0)(aref (state-start_bank else) 1)(aref (state-end_bank else) 0)(aref (state-end_bank else) 1))
  )
)

;; Main function that initializes and calls the solver
;-------------------------------------------------------
; Name:     main
; Params:
; Desc:     -
;--------------------------------------------------------
(defun main()
   (format t "~%Missionaries vs. Cannibals~%")

   (init_node tot_m tot_c boat_size)
   (setf curr_node (vector-pop state-vector))

   (loop while(not(isGoal curr_node)) do

      (find_moves curr_node)

      (setf (gethash (node-id curr_node) visited)curr_node)

      (if (< 0 (length state-vector))
          (sort_nodes state-vector)
      )

      (setf curr_node (vector-pop state-vector))
   )

   (getSolution curr_node)
)
