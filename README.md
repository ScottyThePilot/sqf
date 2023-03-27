# SQF
Work-in-progress for library for SQF, the scripting language of the Arma series.

## Features
- SQF parser
- SQFC compiler

## Missing/Planned Features
- Preprocessor macro handling
- SQFC decompilation
  - This is currently blocked by an issue regarding LZO encoding, namely that there aren't
    any codec libraries I can find that can decode from a buffer/stream without knowing where
    the buffer/stream ends, as SQFC does not store this information.

## Credits
- [dedmen/ArmaScriptCompiler](https://github.com/dedmen/ArmaScriptCompiler)
- [LordGolias/sqf](https://github.com/LordGolias/sqf)
