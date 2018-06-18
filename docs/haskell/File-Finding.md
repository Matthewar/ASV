# File Finding
In its current form the tool does not implement smart file finding, instead it expects all input files to be of the format `<LIB-DIR>/<ENTITY-NAME>.vhd`.
For example, the entity _testentity_ in the _work_ library should be placed in the file `<WORK-DIR>/TESTENTITY.vhd` where `<WORK-DIR>` is the directory passed as a flag to the tool.

By the same reasoning, if an entity has an associated architecture it should be in the same file.
