params ["_args", "_targets", ["_jip", false]];
private _code = { hintSilent parseText _this };
[_args, _code] remoteExecCall ["call", _targets, _jip];
