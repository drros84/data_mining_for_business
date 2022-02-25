# Git instructions

This document is a summary of the most common git commands you will need, in order of how you are likely to use them in a typical workflow:

* **git status**: this shows you whether any changes have been made to the repository that need to be added and committed.
* **git add .**: this adds all untracked files and changes to the staging area (prior to committing).
* **git commit -m "your message"**: this commits your changes to git.
* **git pull origin your_branch**: this pulls down any changes from github (adapt your_branch as necessary). It's best practice to do that before pushing.
* **git push origin your_branch**: this pushes your changes to github.

Once you have pushed your changes to github, you can open a **pull request** to merge changes into the main branch (or any other branches). Typically, it is best practice when working in a team not to work directly on the main branch, and instead work on separate branches that you merge back into main later (or, typically, you could also have a development branch on top of the main branch).

Other useful commands:

* **git checkout -b new_branch**: this creates a new branch called new_branch.
* **git checkout different_branch**: this moves you to the branch called different_branch (which should already have been created).
* **git branch**: this shows you all the local branches that exist.
