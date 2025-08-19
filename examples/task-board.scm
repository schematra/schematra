;; Task Board Example - Functional Components with HTMX Interactivity
;; 
;; This example demonstrates:
;; - Pure functional component composition with Chiccup
;; - HTMX for seamless client-side interactivity
;; - Real-time drag & drop task management
;; - How HTML structure mirrors data structure
;;
;; Run with: csi -s examples/task-board.scm

(import
  (rename srfi-1 (delete srfi1:delete))
  srfi-69 ;; hashtable
  chicken.string
  chicken.random
  chicken.time
  chicken.format
  schematra
  schematra-session
  schematra-body-parser
  chiccup
  )

;; Enable middleware
(use-middleware! (session-middleware "task-board-secret"))
(use-middleware! (body-parser-middleware))

;; =============================================================================
;; Data Structures & Storage
;; =============================================================================

;; In-memory storage (use database in real apps)
(define task-store (make-hash-table))
(define task-counter 0)

;; Task record
(define-record task id title description priority status column created)

;; Column definitions
(define columns 
  '((todo . "To Do")
    (progress . "In Progress") 
    (done . "Done")))

;; Priority levels with colors
(define priority-colors
  '((high . "bg-red-100 text-red-800")
    (medium . "bg-yellow-100 text-yellow-800")
    (low . "bg-green-100 text-green-800")))

;; Sample data
(define (initialize-sample-data!)
  (set! task-counter 0)
  (hash-table-clear! task-store)
  
  (for-each (lambda (task-args) (apply create-task! task-args))
    '(("Design new homepage" "Create wireframes and mockups for the new company homepage" high todo)
      ("Fix login bug" "Users can't log in with special characters in password" high progress)
      ("Write API docs" "Document the REST API endpoints for the mobile team" medium todo)
      ("Deploy to staging" "Push latest changes to staging environment for testing" medium progress)
      ("Code review" "Review John's pull request for the payment system" low todo)
      ("Update dependencies" "Upgrade React and other npm packages to latest versions" low done)
      ("Team meeting notes" "Prepare agenda and take notes for weekly team sync" medium done))))

(define (create-task! title description priority column)
  (set! task-counter (+ task-counter 1))
  (let ((task (make-task task-counter title description priority 'active column (current-seconds))))
    (hash-table-set! task-store task-counter task)
    task))

(define (get-task id)
  (hash-table-ref/default task-store id #f))

(define (get-all-tasks)
  (hash-table-values task-store))

(define (get-tasks-by-column column)
  (filter (lambda (task) (eq? (task-column task) column))
          (get-all-tasks)))

(define (update-task! id updates)
  (let ((task (get-task id)))
    (when task
      (let ((updated-task (apply make-task 
                                 (task-id task)
                                 (or (alist-ref 'title updates) (task-title task))
                                 (or (alist-ref 'description updates) (task-description task))
                                 (or (alist-ref 'priority updates) (task-priority task))
                                 (task-status task)
                                 (or (alist-ref 'column updates) (task-column task))
                                 (task-created task)
                                 '())))
        (hash-table-set! task-store id updated-task)
        updated-task))))

(define (delete-task! id)
  (hash-table-delete! task-store id))

;; =============================================================================
;; Pure Functional UI Components
;; =============================================================================

(define (priority-color priority)
  (alist-ref priority priority-colors))

(define (task-card task)
  `[.bg-white.p-4.rounded-lg.shadow-sm.border.cursor-move.task-card
    (@ (draggable "true") 
       (data-task-id ,(number->string (task-id task)))
       (ondragstart "handleDragStart(event)")
       (ondragend "handleDragEnd(event)"))
    
    [.flex.justify-between.items-start.mb-2
     [h3.font-medium.text-gray-900.text-sm ,(task-title task)]
     [.flex.gap-1
      [button.text-gray-400.hover:text-red-500.text-lg.leading-none
       (@ (hx-delete ,(format "/tasks/~a" (task-id task)))
          (hx-target "closest .task-card")
          (hx-swap "outerHTML")
          (hx-confirm "Delete this task?"))
       "×"]]]
    
    [p.text-xs.text-gray-600.mb-3.leading-relaxed ,(task-description task)]
    
    [.flex.justify-between.items-center
     [span.text-xs.px-2.py-1.rounded-full.font-medium
      (@ (class ,(priority-color (task-priority task))))
      ,(symbol->string (task-priority task))]
     [button.text-xs.text-blue-600.hover:text-blue-800.font-medium
      (@ (hx-get ,(format "/tasks/~a/edit" (task-id task)))
         (hx-target "#modal-content"))
      "Edit"]]])

(define (column-component column-id column-title #!optional filtered-tasks)
  (let* ((all-tasks (or filtered-tasks (get-all-tasks)))
         (tasks (filter (lambda (task) (eq? (task-column task) column-id)) all-tasks)))
    `[.bg-gray-50.rounded-lg.p-4.min-h-96.column
      (@ (data-column ,(symbol->string column-id))
         (ondrop "handleDrop(event)")
         (ondragover "handleDragOver(event)"))
      
      [.flex.justify-between.items-center.mb-4
       [h2.font-semibold.text-gray-800.text-lg ,column-title]
       [span.bg-gray-200.text-gray-600.px-2.py-1.rounded-full.text-sm
        ,(number->string (length tasks))]]
      
      [.space-y-3.min-h-64
       ,@(map task-card tasks)]
      
      [button.w-full.mt-4.py-3.border-2.border-dashed.border-gray-300.rounded-lg.text-gray-500.hover:border-gray-400.transition-colors
       (@ (hx-get ,(format "/tasks/new?column=~a" column-id))
          (hx-target "#modal-content"))
       [.flex.items-center.justify-center.gap-2
        [span.text-lg "+"]
        [span "Add Task"]]]]))

(define (priority-filter current-filter)
  `[.flex.gap-2.items-center
    [label.text-sm.font-medium.text-gray-700 "Filter:"]
    [select.border.border-gray-300.rounded.px-3.py-1.text-sm
     (@ (hx-get "/board")
        (hx-trigger "change")
        (hx-include "this")
        (hx-target "#task-board")
	(hx-swap "outerHTML")
        (name "filter"))
     [option (@ (value "all") ,@(if (equal? current-filter "all") '((selected)) '())) "All Tasks"]
     [option (@ (value "high") ,@(if (equal? current-filter "high") '((selected)) '())) "High Priority"]
     [option (@ (value "medium") ,@(if (equal? current-filter "medium") '((selected)) '())) "Medium Priority"]
     [option (@ (value "low") ,@(if (equal? current-filter "low") '((selected)) '())) "Low Priority"]]])

(define (task-board #!optional (priority-filter "all"))
  (let ((filtered-tasks
         (if (equal? priority-filter "all")
             (get-all-tasks)
             (filter (lambda (task) 
                      (eq? (task-priority task) (string->symbol priority-filter)))
                     (get-all-tasks)))))
    `[.grid.grid-cols-1.md:grid-cols-3.gap-6#task-board
      ,@(map (lambda (column)
               (column-component (car column) (cdr column) filtered-tasks))
             columns)]))

(define (task-form #!optional task column)
  (let* ((is-edit (and task (task-id task)))
         (form-title (if is-edit "Edit Task" "New Task"))
         (form-action (if is-edit 
                         (format "/tasks/~a" (task-id task))
                         "/tasks"))
         (form-method (if is-edit "PUT" "POST")))
    `[div
      [.fixed.inset-0.bg-black.bg-opacity-50.z-40]
      [.fixed.inset-0.flex.items-center.justify-center.z-50.p-4
       [.bg-white.rounded-lg.shadow-xl.w-full.max-w-md
        [.p-6
         [.flex.justify-between.items-center.mb-4
          [h3.text-lg.font-semibold.text-gray-900 ,form-title]
          [button.text-gray-400.hover:text-gray-600.text-xl
           (@ (onclick "document.getElementById('modal-content').innerHTML = ''"))
           "×"]]
         
         [form (@ ,@(if is-edit
                        `((hx-put ,form-action))
                        `((hx-post ,form-action)))
                  (hx-target "#task-board")
                  (hx-swap "outerHTML")
                  (hx-on::after-request "document.getElementById('modal-content').innerHTML = ''"))
          
          [.mb-4
           [label.block.text-sm.font-medium.text-gray-700.mb-1 "Title"]
           [input.w-full.border.border-gray-300.rounded.px-3.py-2.text-sm
            (@ (type "text") (name "title") (required)
               (value ,(if task (task-title task) ""))
               (placeholder "Enter task title"))]]
          
          [.mb-4
           [label.block.text-sm.font-medium.text-gray-700.mb-1 "Description"]
           [textarea.w-full.border.border-gray-300.rounded.px-3.py-2.text-sm.h-24
            (@ (name "description") (required)
               (placeholder "Describe the task"))
            ,(if task (task-description task) "")]]
          
          [.mb-4
           [label.block.text-sm.font-medium.text-gray-700.mb-1 "Priority"]
           [select.w-full.border.border-gray-300.rounded.px-3.py-2.text-sm
            (@ (name "priority") (required))
            [option (@ (value "high") 
                       ,@(if (and task (eq? (task-priority task) 'high)) '((selected)) '()))
             "High"]
            [option (@ (value "medium")
                       ,@(if (and task (eq? (task-priority task) 'medium)) '((selected)) '()))
             "Medium"]
            [option (@ (value "low")
                       ,@(if (and task (eq? (task-priority task) 'low)) '((selected)) '()))
             "Low"]]]
          
          [.mb-6
           [label.block.text-sm.font-medium.text-gray-700.mb-1 "Column"]
           [select.w-full.border.border-gray-300.rounded.px-3.py-2.text-sm
            (@ (name "column") (required))
            ,@(map (lambda (col)
                     `[option (@ (value ,(symbol->string (car col)))
                                ,@(if (eq? (or column (and task (task-column task)) 'todo) (car col)) 
                                      '((selected)) '()))
                       ,(cdr col)])
                   columns)]]
          
          [.flex.gap-3
           [button.flex-1.bg-gray-200.text-gray-800.py-2.px-4.rounded.font-medium.hover:bg-gray-300.transition-colors
            (@ (type "button")
               (onclick "document.getElementById('modal-content').innerHTML = ''"))
            "Cancel"]
           [button.flex-1.bg-blue-600.text-white.py-2.px-4.rounded.font-medium.hover:bg-blue-700.transition-colors
            (@ (type "submit"))
            ,(if is-edit "Update" "Create")]]]]]]]))

(define (main-layout content)
  `[html (@ (lang "en"))
    [head
     [meta (@ (charset "utf-8"))]
     [meta (@ (name "viewport") (content "width=device-width, initial-scale=1"))]
     [title "Task Board - Schematra + HTMX Example"]
     [script (@ (src "https://cdn.tailwindcss.com"))]
     [script (@ (src "https://unpkg.com/htmx.org@1.9.10"))]
     [style "
       .task-card.dragging { opacity: 0.5; transform: rotate(5deg); }
       .column.drag-over { background-color: #e0f2fe; }
       .modal-enter { animation: modalEnter 0.2s ease-out; }
       @keyframes modalEnter {
         from { opacity: 0; transform: scale(0.95); }
         to { opacity: 1; transform: scale(1); }
       }"]]
    [body.bg-gray-100.min-h-screen
     [.container.mx-auto.py-8.px-4
      ,content
      
      ;; Modal container
      [\#modal-content]]
     
     ;; Drag & Drop JavaScript
     [script "
       let draggedElement = null;
       
       function handleDragStart(e) {
         draggedElement = e.target.closest('.task-card');
         draggedElement.classList.add('dragging');
         e.dataTransfer.effectAllowed = 'move';
         e.dataTransfer.setData('text/html', draggedElement.outerHTML);
       }
       
       function handleDragEnd(e) {
         if (draggedElement) {
           draggedElement.classList.remove('dragging');
           draggedElement = null;
         }
       }
       
       function handleDragOver(e) {
         e.preventDefault();
         e.dataTransfer.dropEffect = 'move';
         e.currentTarget.classList.add('drag-over');
       }
       
       function handleDrop(e) {
         e.preventDefault();
         e.currentTarget.classList.remove('drag-over');
         
         if (draggedElement) {
           const taskId = draggedElement.dataset.taskId;
           const newColumn = e.currentTarget.dataset.column;
           
           // Move task via HTMX
           htmx.ajax('PUT', `/tasks/${taskId}/move`, {
             values: { column: newColumn },
             target: '#task-board',
             swap: 'outerHTML'
           });
         }
       }
       
       // Remove drag-over class when dragging leaves
       document.addEventListener('dragleave', function(e) {
         if (e.target.classList.contains('column')) {
           e.target.classList.remove('drag-over');
         }
       });
     "]]])

;; =============================================================================
;; Routes
;; =============================================================================

(define (get-filter-param)
  (or (alist-ref 'filter (current-params)) "all"))

;; Main board view
(get ("/")
     (initialize-sample-data!)
     (ccup->html
      (main-layout
       `[div
         [.flex.justify-between.items-center.mb-8
          [div
           [h1.text-3xl.font-bold.text-gray-900 "Task Board"]
           [p.text-gray-600.mt-1 "Functional components with HTMX interactivity"]]
          ,(priority-filter (get-filter-param))]
         
         ,(task-board (get-filter-param))
         
         [.mt-8.text-center.text-sm.text-gray-500
          [p "This example showcases:"]
          [ul.mt-2.space-y-1
           [li "• Pure functional component composition with Chiccup"]
           [li "• HTMX for seamless interactivity without page refreshes"]  
           [li "• Drag & drop task management"]
           [li "• HTML structure that mirrors data structure"]]]])))

;; Board updates (for filtering)
(get ("/board")
     (ccup->html (task-board (get-filter-param))))

;; New task form
(get ("/tasks/new")
     (let ((column (string->symbol (or (alist-ref 'column (current-params)) "todo"))))
       (ccup->html (task-form #f column))))

;; Edit task form  
(get ("/tasks/:id/edit")
     (let* ((id (string->number (alist-ref "id" (current-params) equal?)))
            (task (get-task id)))
       (if task
           (ccup->html (task-form task))
           (ccup->html `[div "Task not found"]))))

;; Create new task
(post ("/tasks")
      (let ((title (alist-ref 'title (current-params)))
            (description (alist-ref 'description (current-params)))
            (priority (string->symbol (alist-ref 'priority (current-params))))
            (column (string->symbol (alist-ref 'column (current-params)))))
        (create-task! title description priority column)
        (ccup->html (task-board))))

;; Update task
(put ("/tasks/:id")
     (let* ((id (string->number (alist-ref "id" (current-params) equal?)))
            (title (alist-ref 'title (current-params)))
            (description (alist-ref 'description (current-params)))
            (priority (string->symbol (alist-ref 'priority (current-params))))
            (column (string->symbol (alist-ref 'column (current-params))))
            (updates `((title . ,title)
                      (description . ,description)
                      (priority . ,priority)
                      (column . ,column))))
       (update-task! id updates)
       (ccup->html (task-board))))

;; Move task between columns
(put ("/tasks/:id/move")
     (let* ((id (string->number (alist-ref "id" (current-params) equal?)))
            (new-column (string->symbol (alist-ref 'column (current-params))))
            (updates `((column . ,new-column))))
       (update-task! id updates)
       (ccup->html (task-board))))

;; Delete task
(delete ("/tasks/:id")
        (let ((id (string->number (alist-ref "id" (current-params) equal?))))
          (delete-task! id)
          ""))

;; =============================================================================
;; Server Setup
;; =============================================================================

(schematra-install)
(schematra-start)

(print "\n🚀 Task Board Example running at http://localhost:8080")
(print "\nThis example demonstrates:")
(print "  • Functional component composition with Chiccup")
(print "  • HTMX for seamless client-side updates")
(print "  • Drag & drop interactivity")
(print "  • How HTML structure mirrors data structure")
(print "\nTry creating, editing, and moving tasks around!")
