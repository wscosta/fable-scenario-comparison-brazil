---
name: session-close
description: End-of-session learning capture. Reviews what happened during the session, identifies learning moments (corrections, validated approaches, revealed preferences, debugging strategies, reusable structures), and saves relevant memories. Run at the end of any substantial working session, or when the user says they're done for now, wrapping up, or asks to close the session.
---

# Session Close

Run this at the end of a substantial working session to capture what we learned.

## Steps

1. **Briefly review what happened this session** — what problems did we solve, what approaches did we try, what worked or didn't?

2. **Check for learning moments** — scan for any of these that occurred:
   - A correction (explicit or implicit) to something I said or did
   - An approach that worked unusually well or failed unexpectedly
   - A preference revealed by the user's choices, not just their words
   - A pattern appearing for the second time
   - A debugging strategy worth remembering
   - A reusable structure or template that emerged
   - **A recall miss**: a memory existed that should have surfaced for a situation this session, but didn't — usually a sign the memory's `description:` is under-specified for the situation. Resolve by sharpening the existing memory's description, not by creating a new one. If you can't recall noticing such a miss, that's fine — quiet pass.

3. **For each moment worth saving**, write a memory file (feedback, project, or user type as appropriate). Lead with the rule/fact, then **Why:** and **How to apply:** lines. Save the *reasoning*, not just the event.

4. **Check for reusable structures** — did we build or refine a project template (pipeline, validation system, task framework) that should be extracted for future projects? If yes, save it to memory with enough detail to redeploy.

5. **Update CLAUDE.md if needed** — review what was built or decided this session and check whether CLAUDE.md reflects the current state of the project. Update any section that is outdated, incomplete, or missing. Only add what isn't already described — don't duplicate.

6. **Update MEMORY.md** with any new entries.

7. **Report back**: list what you saved and whether CLAUDE.md was updated (or note "nothing worth saving this session" if that's accurate). Don't force it — a quiet session is fine.
