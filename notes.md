# linking


the CFG does not perform correct linking, because it ignores static/exported modifiers

it does not link two functions (ever), so local calls are always resolved correctly, but in the case
 that a function is defined both statically and externally in separate files, the linking mechanism links
 against both(!).

it would be more accurate if the CFG linker also inspects the .interface files to link only functions
 marked as exported in conditions when they are exported.


# declarations

functions may call declarations even though a function with the same name is present.
this is the case, because during linking a reference to a declaration is replaced only
 for those configurations where the target is present. for all other configurations, the
 old target remains. typically this occurs only in configurations that are excluded by the
 feature model (the linker filters all unsatisfiable edges but does not consider the feature
 model)