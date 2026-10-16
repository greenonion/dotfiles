---
description: Implement a plan task by task, reviewing and committing each task
argument-hint: "<filename>"
---
Read the plan file "$1" carefully. Preserve the plan's intent and task order; ask before material deviations. Start with the first unfinished task, verifying earlier tasks against the code and tests rather than trusting plan checkboxes alone. If no filename was supplied, ask for one before proceeding. Resolve relative filenames from the current working directory.

For each task, in plan order:

1. Establish the task's acceptance criteria and record the starting repository state so task changes can be distinguished from pre-existing or concurrent work. Implement the task following repository instructions, using red/green tests for new behavior, and run the relevant tests and required checks.
2. Call a `code-reviewer` subagent using exactly `gpt-6.1-sol` with `medium` thinking. Give it the plan, current task, acceptance criteria, validation results, and the complete current-task diff, including staged, unstaged, and new files—not just whatever is currently unstaged. Distinguish unrelated changes and provide access to surrounding code as needed. The reviewer must be read-only and report actionable findings with file references, or explicitly report no findings. If the agent or requested model/thinking configuration is unavailable, stop and report the blocker rather than substituting another configuration.
3. Assess each finding against the plan and code. Fix valid findings; explain rejected findings with evidence to the reviewer rather than silently dismissing them. Rerun affected tests and checks after fixes, then call the code-reviewer again with the same model and thinking level, the updated complete task diff, and the finding dispositions. Repeat until a completed review reports no unresolved findings. If reviews repeat resolved findings, disagreement persists, or the loop stops making progress, stop and ask; do not silently impose a review limit and commit anyway. Also stop if resolving a finding requires clarification or additional approval.
4. Commit only when the task's acceptance criteria are met, relevant tests and required checks pass, and the completed review has no unresolved findings. Reviewer approval alone is not proof of completion. Commit only changes belonging to this task with a descriptive commit message. This command explicitly authorizes these per-task commits, but not pushing, destructive Git operations, or committing unrelated work. Preserve pre-existing and concurrent changes; if task changes cannot be safely separated, stop and ask.
5. Repeat this process with the next task until the entire plan is implemented.

Keep all other approval and safety boundaries intact. If the plan is ambiguous or conflicts with repository instructions, ask before deviating. At the end, summarize completed tasks, commits, validation, and any remaining blockers.
