# Design of Grubber

### Preamble

When reading this, keep in mind that it only provides a snapshot of thoughts that most likely have not been ordered well enough. In case of doubt, check the date this file was last edited and then open an Issue about it.

### What is a build tool

For our purpose, a build tool is in charge of keeping an external, mutable, state consistent and up to date. 'External' because the managed state outlives the build system. 'Mutable' to say that the *effects* of build rules must be carefully kept in check to commute with one-another for any sensible approach of executing them. Finally 'consistent' as such that a user requesting a subset of the state should see a self-consistent view of that subset - not necessarily the whole state.

The managed state is most often subdivided into seperate pieces, each of which is to be updated by exactly one *rule*. This rule is the *solely* responsible for writing to its part of the state, and can only read from other parts.

**`make`** manages the filesystem. For each file there can only be one rule declaring the steps for building it. Actually, `GNU make` also support "Grouped Targets" that are responsible for a collection of files - in fact Pattern Rules are always treated as Grouped Targets. A disadvantage is then that it is not possible inside the rule to detect which subset of the grouped targets were requested or which subset of a rule's dependencies triggered an update. To solve the latter problem, "Double-Colon Rules" exist, but are hard to use correctly.

Consistency has a bunch of obvious, and not so obvious, implications. Clearly, one has to build dependencies *before* building the dependent. Trouble starts with parallel builds where a good build system provides dependencies just in time for when they are needed.
