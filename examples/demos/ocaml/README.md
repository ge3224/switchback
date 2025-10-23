# OCaml Demo - Task Workflow with Pattern Matching

**Type-safe state machines + Switchback = Instant navigation with compile-time guarantees**

This demo showcases Switchback's **instant navigation and optimistic updates** using OCaml's pattern matching and algebraic data types for bulletproof state management.

## What Makes This Demo Special

A focused demonstration of type-safe workflows:

- ✅ **Pattern matching for state transitions** - Compiler ensures all cases are handled
- ✅ **Algebraic Data Types** - Invalid states are impossible to construct
- ✅ **Instant navigation** - Zero-reload page transitions with app.visit()
- ✅ **Optimistic updates** - UI responds instantly while server validates
- ✅ **Zero framework dependencies** - Pure OCaml stdlib (Unix + Str) + vanilla JS

## Try It Out

Run the full stack (OCaml server + frontend build) in Docker:

```bash
cd examples/demos/ocaml
docker-compose up
```

Open http://localhost:8000

**That's it!** Docker Compose handles the OCaml compilation, TypeScript bundling, and server startup automatically.

## How It Works

### The Core Pattern

The server detects the `X-Switchback` header and responds differently:

```ocaml
let is_switchback =
  List.exists (fun (k, _) -> k = "X-Switchback") headers in

(* Dual response strategy *)
if is_switchback then
  respond_json conn task_json  (* JSON for navigation *)
else
  respond_html conn task_json  (* HTML for first load *)
```

### The Request Flow

**1. Initial Load** (`GET /tasks` without header)
```
Browser → OCaml Server
         ← Full HTML with <script>window.initialPage = {...}</script>
Client-side app hydrates with task list (no loading state!)
```

**2. State Transition** (`POST /api/tasks/1/transition` with optimistic update)
```
User clicks "Approve"
Client → Updates UI immediately (optimistic)
      → OCaml Server (POST with action: "approve")
OCaml pattern matches: (InReview _, "approve") -> Valid ✓
      ← JSON: {"success": true, "task": {...}}
Client confirms transition (border becomes solid)
```

**3. Page Navigation** (`GET /task/1` with `X-Switchback: true`)
```
User clicks task card
Client → OCaml Server (with X-Switchback header)
       ← JSON: {"component":"TaskDetail","props":{...}}
Client renders detail view (no reload!)
```

### The Architecture

**Backend (server.ml):**
- Routes: `/`, `/task/:id`, `/published/*`, `/api/tasks/*`
- Pattern matching validates all state transitions
- ADTs model workflow states with type-safe data
- Dual response: HTML wrapper or JSON based on `X-Switchback` header

**Frontend (app.ts):**
- Components: Main, TaskDetail, PublishedTask
- Instant navigation with app.visit()
- Optimistic updates with server validation
- Auto-generated publish URLs from task titles

## Understanding OCaml's Superpowers

### Algebraic Data Types - "Smart Enums"

**The problem with traditional approaches:**

```javascript
// JavaScript - easy to make mistakes!
task.status = "in_review"
task.reviewer = "Alice"  // But what if status is "draft"? No reviewer yet!
task.url = ???           // Only exists when published...
```

Issues:
- Which fields exist for which status?
- Easy to forget to set/check fields
- Typos in status strings (`"aprove"` vs `"approve"`)

**OCaml's solution - States with attached data:**

```ocaml
type workflow_state =
  | Draft                                                    (* Just a state, no data *)
  | InReview of { reviewer: string; submitted_at: float }  (* State + reviewer info *)
  | Approved of { reviewer: string; approved_at: float }   (* State + approval info *)
  | Published of { reviewer: string; published_at: float; url: string }  (* State + URL! *)
  | Rejected of { reviewer: string; reason: string; rejected_at: float }
```

**What this means:**
- Each state can have **different fields**
- `Draft` has no extra data
- `InReview` automatically includes who's reviewing and when
- `Published` is the **only** state with a `url` field
- Compiler prevents accessing fields that don't exist for a state

**Real example from server.ml:**

```ocaml
let state_to_json = function
  | Draft -> {|{"status":"draft","label":"Draft"}|}
  | InReview { reviewer; submitted_at } ->
      (* Can ONLY access reviewer/submitted_at in this state *)
      sprintf {|{..."reviewer":"%s","submitted_at":%.0f}|} reviewer submitted_at
  | Published { url; _ } ->
      (* Can ONLY access url in Published state *)
      sprintf {|{..."url":"%s"}|} url
```

Compiler **forces** you to handle all cases and **prevents** accessing non-existent fields.

### Pattern Matching - The State Machine

Pattern matching is like `switch/case` but checks **multiple values at once** and **ensures all cases are handled**.

**The state machine:**

```ocaml
(* Check BOTH current state AND action *)
match (task.state, action) with
  (* Draft can only transition to InReview *)
  | (Draft, "review") ->
      let new_state = InReview { reviewer; submitted_at = now } in
      Success updated

  (* InReview can transition to Approved or Rejected *)
  | (InReview _, "approve") ->
      let new_state = Approved { reviewer; approved_at = now } in
      Success updated
  | (InReview _, "reject") ->
      let new_state = Rejected { reviewer; reason; rejected_at = now } in
      Success updated

  (* Published is final - no transitions allowed! *)
  | (Published _, _) ->
      InvalidTransition "Cannot modify published tasks"

  (* Catch-all for invalid transitions *)
  | _ ->
      InvalidTransition "Cannot perform this action"
```

**What's happening:**
- `(Draft, "review")` - Only allow if state is `Draft` AND action is `"review"`
- `(InReview _, "approve")` - The `_` means "I don't care about the data, just that it's InReview"
- `(Published _, _)` - Published with ANY action gets rejected
- Compiler error if you forget a case!

**Compared to JavaScript/TypeScript:**

```javascript
// Easy to forget cases, no compiler help
function transitionTask(task, action) {
  if (task.status === "draft" && action === "review") {
    task.status = "in_review";
    task.reviewer = reviewer;  // Oops, forgot submitted_at!
  } else if (task.status === "in_review" && action === "approve") {
    task.status = "approved";
  }
  // What if we forget to handle "reject"?
  // What if we typo "aprove"?
  // No compiler to catch these!
}
```

### Pattern Matching Everywhere

**Available actions per state:**

```ocaml
let get_available_actions = function
  | Draft -> ["review"]
  | InReview _ -> ["approve"; "reject"]
  | Approved _ -> ["publish"]
  | Rejected _ -> ["draft"]
  | Published _ -> []  (* Final state! *)
```

Add a new state? Compiler errors until you handle it everywhere!

### Safe Refactoring

Add a 6th state? The compiler shows **every** place you need to update:

```ocaml
type workflow_state =
  | Draft
  | InReview of { reviewer: string; submitted_at: float }
  | InTesting of { tester: string; test_suite_url: string }  (* NEW! *)
  | Approved of { reviewer: string; approved_at: float }
  | Published of { reviewer: string; published_at: float; url: string }
  | Rejected of { reviewer: string; reason: string; rejected_at: float }
```

Compiler errors guide you:
- `state_to_json` - Missing InTesting case
- `transition_task` - What transitions are valid?
- `get_available_actions` - What actions are available?

Fix all errors → refactoring complete. Nothing missed.

## Key Files

```
ocaml/
├── server.ml              # OCaml HTTP server with pattern matching
│                          # Routes, state machine logic, JSON serialization
├── app.ts                 # Switchback frontend (Main, TaskDetail, PublishedTask)
├── dune-project            # Dune build configuration
├── package.json           # Frontend build (Vite + TypeScript)
├── docker-compose.yml     # One command to run everything
└── README.md              # You are here
```

## Task Workflow States

```
                    ┌─────────┐
                    │  Draft  │
                    └────┬────┘
                         │ review
                         ↓
                  ┌──────────────┐
                  │  In Review   │
                  └──┬────────┬──┘
          approve →  │        │  ← reject
                     ↓        ↓
              ┌──────────┐  ┌──────────┐
              │ Approved │  │ Rejected │
              └────┬─────┘  └────┬─────┘
         publish → │              │  ← draft (resubmit)
                   ↓              ↓
            ┌────────────┐   ┌─────────┐
            │ Published  │   │  Draft  │
            └────────────┘   └─────────┘
              (FINAL)
```

## Extending This Demo

**Add database persistence:** Replace `tasks : task list ref` with PostgreSQL/SQLite using Caqti

**Add WebSockets:** Broadcast state changes to all connected clients with real-time updates

**Add authentication:** Use OCaml's Dream framework for session management and user roles

**Add more states:** Extend the ADT (e.g., `InTesting`, `Scheduled`) - compiler guides the refactor

## Running Natively (Without Docker)

**Prerequisites:**
- [OCaml 5.2+](https://ocaml.org/install) for the server
- [Node.js](https://nodejs.org/) for frontend build (or use Docker)

**Steps:**
```bash
cd examples/demos/ocaml

# 1. Install OCaml dependencies
opam install dune

# 2. Build frontend with Node
npm install
npm run build

# 3. Compile OCaml server
dune build server.exe

# 4. Run server
dune exec ./server.exe
```

Open http://localhost:8000

## Performance

OCaml compiles to native code with zero-cost abstractions:
- Minimal memory footprint (~15MB for this server)
- Pattern matching compiled to efficient jump tables
- Production build: `dune build --profile=release`

## What This Demonstrates

**OCaml:** Type-safe state machines with pattern matching prevent invalid transitions at compile time

**Switchback:** Instant navigation (app.visit), optimistic updates, auto-generated publish URLs, shareable published pages

**Together:** Backend correctness + instant UX with zero framework dependencies
