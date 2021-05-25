My custom mathematica config. This will be used in other Mathematica files in all of my other projects. Clone this in your .Mathematica directory at ```$HOME``` to use it. Can be used for customizing individual projects as well. Please refer to Mathematica Documentation to know about how to set it up.

## Subscript as Indexed
Code summary:

![Code](CodeExample.png "Example Code")

Usage:

![Example Usage](Example.png "Example Usage")

## Alert using sound after something is done
Code summary:
```
Alert[]:=EmitSound[Sound[SoundNote[]]]
```
Usage:
```
LongCalculation[Expr];Alert[]
```

## Remote login
See [Stack Exchange Answer](https://mathematica.stackexchange.com/questions/28274/remote-kernel-error-mleconnect).
