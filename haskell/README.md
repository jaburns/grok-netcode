### grok-netcode

To build on any platform first [install stack](https://docs.haskellstack.org/en/stable/README) and then run
the following from the project root:

```
stack build
cp win64_freeglut\glut32.dll .  # <-- Windows only
stack exec grok-netcode
```